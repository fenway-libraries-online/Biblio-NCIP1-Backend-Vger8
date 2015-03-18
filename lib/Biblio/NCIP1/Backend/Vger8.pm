package Biblio::NCIP1::Backend::Vger8;

use strict;
use warnings;

use vars qw($VERSION);

$VERSION = '0.03';

use Biblio::Vger::DBI;
use Biblio::SIP2::Vger8;
use Biblio::NCIP1::Constants qw(:requests :errors);
use Biblio::NCIP1::Config;
use YAML qw(DumpFile LoadFile);

use constant HOOK_HOLD_CREATED   => 'hold-created';
use constant HOOK_HOLD_CANCELLED => 'hold-cancelled';

sub fail($);

my $dbh;
my %sql2sth;

my $sql_item_home_circ_location = q{
    SELECT l.location_code
    FROM   circ_policy_locs cpl,
           location l
    WHERE  cpl.location_id = l.location_id
    AND    cpl.circ_location = 'Y'
    AND    cpl.circ_group_id IN (
                SELECT cpl.circ_group_id
                FROM   circ_policy_locs cpl,
                       location l,
                       item i,
                       item_barcode ib
                WHERE  cpl.location_id = l.location_id
                AND    i.perm_location = l.location_id
                AND    i.item_id = ib.item_id
                AND    ib.item_barcode = :p1
           )
};
my $sql_patron_by_id = q{
    SELECT p.last_name, p.first_name, b.patron_barcode, p.patron_pin
    FROM   patron p, patron_barcode b
    WHERE  p.patron_id = b.patron_id
    AND    b.patron_id = ?
    AND    b.barcode_status = 1
};
my $sql_patron_by_non_numeric_id = q{
    SELECT p.patron_id, p.last_name, p.first_name, b.patron_barcode, p.patron_pin
    FROM   patron p, patron_barcode b
    WHERE  p.patron_id = b.patron_id
    AND    b.patron_barcode = ?
    AND    b.barcode_status = 1
};
my $sql_patron_by_barcode = q{
    SELECT p.last_name, p.first_name, b.patron_id
    FROM   patron p, patron_barcode b
    WHERE  p.patron_id = b.patron_id
    AND    b.patron_barcode = ?
    AND    b.barcode_status = 1
};
my $sql_patron_address = q{
    SELECT a.address_line1, a.city, a.state_province, a.zip_postal, ph.phone_number
    FROM   patron_address a LEFT JOIN patron_phone ph ON a.address_id = ph.address_id
    WHERE  a.patron_id = ?
    AND    a.address_type = 1  /* permanent */
};
my $sql_patron_email = q{
    SELECT a.address_line1
    FROM   patron_address a
    WHERE  a.patron_id = ?
    AND    a.address_type = 3
    AND    SYSDATE >= a.effect_date
    AND    (a.expire_date IS NULL OR a.expire_date > SYSDATE)
};
my $sql_item_barcode_to_id = q{
    SELECT item_id
    FROM   item_barcode
    WHERE  barcode_status = 1  /* active */
    AND    item_barcode = ?
};
## my $sql_bib_id_and_item_barcode_to_item_id = q{
##     SELECT ib.item_id
##     FROM   bib_mfhd bm, mfhd_item mi, item_barcode ib
##     WHERE  bm.mfhd_id = mi.mfhd_id
##     AND    mi.item_id = ib.item_id
##     AND    bi.barcode_status = 1  /* active */
##     AND    bm.bib_id = ?
##     AND    ib.item_barcode = ?
## };
my $sql_item_id_to_barcode = q{
    SELECT item_barcode
    FROM   item_barcode
    WHERE  barcode_status = 1  /* active */
    AND    item_id = ?
};
my $sql_item_barcode_to_bib_id_location_and_item_id = q{
    SELECT bm.bib_id, l.location_code, ib.item_id
    FROM   bib_mfhd bm, mfhd_item mi, mfhd_master mm, location l, item_barcode ib
    WHERE  bm.mfhd_id = mi.mfhd_id
    AND    mi.mfhd_id = mm.mfhd_id
    AND    mm.location_id = l.location_id
    AND    mi.item_id = ib.item_id
    AND    ib.barcode_status = 1  /* active */
    AND    ib.item_barcode = ?
};
my $sql_location_limit_group_and_bibid_to_lendable_holdings = q{
    SELECT bm.bib_id, l.location_code, ib.item_barcode, ib.item_id
    FROM   bib_mfhd bm,
           mfhd_master mm,
           location l,
           location_limit ll,
           location_limit_locs lll,
           mfhd_item mi,
           item_status s,
           item_barcode ib
    WHERE  bm.mfhd_id = mm.mfhd_id
    AND    mm.location_id = l.location_id
    AND    l.location_id  = lll.location_id
    AND    ll.location_limit_id = lll.location_limit_id
    AND    bm.mfhd_id = mi.mfhd_id
    AND    mi.item_id = s.item_id
    AND    mi.item_id = ib.item_id
    AND    s.item_status in ( 1, 11 )
    AND    ll.limit_code = ?
    AND    bm.bib_id = ?
};
my $sql_location_limit_group_and_isbn_to_lendable_holdings = q{
    SELECT bi.bib_id, l.location_code, ib.item_barcode, ib.item_id
    FROM   bib_index bi,
           bib_mfhd bm,
           mfhd_master mm,
           location l,
           location_limit ll,
           location_limit_locs lll,
           mfhd_item mi,
           item_status s,
           item_barcode ib
    WHERE  bi.bib_id = bm.bib_id
    AND    bm.mfhd_id = mm.mfhd_id
    AND    mm.location_id = l.location_id
    AND    l.location_id  = lll.location_id
    AND    ll.location_limit_id = lll.location_limit_id
    AND    bm.mfhd_id = mi.mfhd_id
    AND    mi.item_id = s.item_id
    AND    mi.item_id = ib.item_id
    AND    s.item_status in ( 1, 11 )
    AND    bi.index_code = 'ISB3'
    AND    ll.limit_code = ?
    AND    bi.normal_heading = ?
};

sub new {
    my $cls = shift;
    my $self = bless { @_ }, $cls;
    my $config = $self->{'config'} ||= {};
    my $config_file = $self->{'config_file'};
    if ($config_file) {
        %$config = (
            %$config,
            %{ Biblio::NCIP1::Config->parse($config_file) },
        );
        my $config_dir = $config_file;
        $config_dir =~ s{/[^/]+$}{};
        while (my ($key, $val) = each %{ $config->{'include'} }) {
            $config->{$key} = Biblio::NCIP1::Config->parse("$config_dir/$val");
        }
        delete $config->{'include'};
    }
    return $self;
}

sub startup {
    my ($self) = @_;
    my $config_file = $self->{'config_file'} || '(undefined)';
    print STDERR "** backend config file: $config_file\n";
}

sub teardown { }

# --- Message handlers

sub CreateUser {
    my ($self, $req) = @_;
    fail ERR_UNSUPPORTED_SERVICE;
}

sub LookupUser {
    my ($self, $req) = @_;
    my $user = $req->{user} || fail ERR_ELEMENT_RULE_VIOLATED;
    my ($uid, $barcode, $pin) = @$user{qw(id barcode pin)};
    ## Require valid PIN only if a PIN is supplied -- fail "invalid request: no PIN" if !defined $uid && !defined $pin;
    $user = $self->find_user(%$user) || fail ERR_UNKNOWN_USER;
    $uid ||= $user->{uid};
    $barcode ||= $user->{barcode};
    my ($stored_pin, $last) = @$user{qw(pin last)};
    if (!defined $uid) {
        fail ERR_ELEMENT_RULE_VIOLATED if !$barcode;
        fail ERR_ELEMENT_RULE_VIOLATED if !defined $pin || !length $pin;
    }
    if (defined $pin) {
        # Check the PIN that the user provided -- it must match *either*
        # the PIN stored in the Voyager database *or* their last name.
        # Comparisons are case-insensitive.
        $pin = lc $pin;
        $stored_pin = defined $stored_pin ? lc $stored_pin : '';
        $last       = defined $last       ? lc $last       : '';
        if ($stored_pin ne $pin && $last ne $pin) {
            fail ERR_USER_AUTHENTICATION_FAILED;
        }
    }
    # Validate the user using SIP
    my $sip = $self->sip(patron => $barcode);
    return { user => $user };
}

sub RequestItem {
    my ($self, $req) = @_;
    # Get user info
    my $user = $self->find_user(%{ $req->{user} || fail ERR_ELEMENT_RULE_VIOLATED }) || fail ERR_UNKNOWN_USER;
    my $user_barcode = $user->{barcode};
    fail ERR_ELEMENT_RULE_VIOLATED if !defined $user_barcode;
    # Find bibitems (if any)
    my $agency = $req->{recipient};
    my $bibitems = $self->find_bibitems($agency, %{ $req->{item} || $req->{bibitem} || fail ERR_ELEMENT_RULE_VIOLATED });
    fail ERR_UNKNOWN_ITEM if !$bibitems || !%$bibitems;
    # We found something
    my %res;
    my $sip = $self->sip(patron => $user_barcode);
    # Score each bibitem
    my @bibitems;
    foreach (values %$bibitems) {
        foreach my $item (@$_) {
            push @bibitems, [ score_bibitem($item), $item ];
        }
    }
    # Try to place a hold on each bibitem in turn, stopping as soon as one works
    foreach my $bibitem (map { $_->[1] } sort { $b->[0] <=> $a->[0] } @bibitems) {
        my $bib = $bibitem->{bib};
        my $item = $bibitem->{item};
        my $item_barcode = $item->{barcode};
        my $bib_id = $bib->{id};
        fail ERR_ELEMENT_RULE_VIOLATED if !defined $item_barcode;
        fail ERR_ELEMENT_RULE_VIOLATED if !defined $bib_id;
        %res = $sip->hold($user_barcode, $bib_id, $item_barcode);
        if ($res{ok}) {
            # XXX Fill in bib description
            # XXX Log details for notification purposes
            my %retval = (
                request => $req->{request},
                user => $user,
                item => $item,
                bib  => $bib,
            );
            $self->hook_request_created(%retval);
            return \%retval;
        }
    }
    fail ERR_UNKNOWN_ITEM;
}

sub CancelRequestItem {
    my ($self, $req) = @_;
    # Find user and item
    my $user = $self->find_user(%{ $req->{user} || fail ERR_ELEMENT_RULE_VIOLATED }) || fail ERR_UNKNOWN_USER;
    my $user_barcode = $user->{barcode} || fail ERR_UNKNOWN_USER;
    my ($request, $bib, $bib_id, $item, $item_barcode);
    if ($req->{request}) {
        $request = $self->find_request(%{ $req->{request} }) || fail ERR_UNKNOWN_REQUEST;
        $item = $request->{item}         || fail ERR_UNKNOWN_ITEM;
        $item_barcode = $item->{barcode} || fail ERR_UNKNOWN_ITEM;
        $bib = $request->{bib}           || fail ERR_UNKNOWN_ITEM;
        $bib_id = $bib->{id};
    }
    elsif ($req->{item}) {
        $request = {};
        $item = $self->find_item(%{ $req->{item} }) || fail ERR_UNKNOWN_ITEM;
        $item_barcode = $item->{barcode} || fail ERR_UNKNOWN_ITEM;
        ($bib_id) = $self->query1($sql_item_barcode_to_bib_id_location_and_item_id, $item_barcode);
        $bib = {
            id => $bib_id,
        };
    }
    # We'll need the bib ID, too
    fail ERR_UNKNOWN_ITEM if !defined $bib_id;
    # Punt to SIP
    my $sip = $self->sip(patron => $user_barcode);
    my %res = $sip->cancel_hold($user_barcode, $bib_id, $item_barcode);
    fail ERR_UNKNOWN_REQUEST if !$res{ok};  # XXX
    my %retval = (
        request => $request,
        user => $user,
        item => $item,
    );
    $self->hook_request_cancelled(%retval, bib => $bib);
    return \%retval;
}

sub CheckOutItem {
    my ($self, $req) = @_;
    my $user = $self->find_user(%{ $req->{user} || fail ERR_ELEMENT_RULE_VIOLATED }) || fail ERR_UNKNOWN_USER;
    my $item = $self->find_item(%{ $req->{item} || fail ERR_ELEMENT_RULE_VIOLATED }) || fail ERR_UNKNOWN_ITEM;
    my $patron_barcode = $user->{barcode} || fail ERR_ELEMENT_RULE_VIOLATED;
    my $item_barcode   = $item->{barcode} || fail ERR_ELEMENT_RULE_VIOLATED;
    my $sip = $self->sip(patron => $patron_barcode);
    my %res = $sip->checkout($patron_barcode, $item_barcode);
    fail ERR_USER_INELIGIBLE_TO_CHECK_OUT_THIS_ITEM if !$res{ok};
    return {
        user => $user,
        item => $item,
        due_date => sipdate2iso8601($res{due_date}),
    };
}

sub AcceptItem {
    my ($self, $req) = @_;
    # We are the borrowing agency
    # Use SIP to create bib, MFHD, and item records
    my $bibdesc  = $req->{bibdesc};
    my $title = $bibdesc->{title};
    #my $itemdesc = $req->{itemdesc};
    my $reqnum = $req->{request}{id} || fail ERR_ELEMENT_RULE_VIOLATED;
    my $user   = $req->{user};
    $user = $self->find_user(%$user) || fail ERR_UNKNOWN_USER;
    my $patron_barcode = $user->{barcode} || fail ERR_UNKNOWN_USER;
    # Create the bib, MFHD, and item
    my $item_barcode_template = $self->{config}{voyager}{item_barcode_format};
    my $item_barcode = sprintf($item_barcode_template, $reqnum);
    my ($item_id) = $self->query1($sql_item_barcode_to_id, $item_barcode);
    fail ERR_DUPLICATE_ITEM if defined $item_id;
    # my $loc = $self->{config}{sip}{location};
    my $sip = $self->sip;
    # my $sip = $self->sip(location => $loc, patron => $patron_barcode);
    my %res = $sip->create($item_barcode, $title, $reqnum);
    fail ERR_CANNOT_ACCEPT_ITEM if !$res{ok};
    ($item_id) = $self->query1($sql_item_barcode_to_id, $item_barcode);
    my $bib_id = $res{bib_id};
    my $loc = $self->item_home($item_barcode);
    # XXX Hack -- Voyager 9-specific but shouldn't break in other versions
    $sip->disconnect;
    $sip = $self->sip(patron => $patron_barcode);
    %res = $sip->hold($patron_barcode, $bib_id, $item_barcode, $loc);
    fail ERR_USER_INELIGIBLE_TO_REQUEST_THIS_ITEM if !$res{ok};
    return {
        item => {
            id => $item_id,
            barcode => $item_barcode,
        },
        request => {
            id => $reqnum,
        }
    };
}

sub CheckInItem {
    my ($self, $req) = @_;
    my $item = $self->find_item(%{$req->{item}});
    my $item_barcode = $item->{barcode} || fail ERR_ELEMENT_RULE_VIOLATED;
    my $loc = $self->item_home($item_barcode);
    my %res;
    my $sip = $self->sip(location => $loc);
    for (1..2) {
        %res = $sip->checkin($item_barcode);
        return { item => $item } if $res{ok};
    }
    fail ERR_ITEM_NOT_CHECKED_OUT;
}

sub RenewItem {
    my ($self, $req) = @_;
    my ($user, $msg) = $self->validate_user(%{ $req->{user} || fail ERR_ELEMENT_RULE_VIOLATED });
    fail ERR_ITEM_NOT_CHECKED_OUT_TO_THIS_USER if !$user;
    my $item = $self->find_item(%{$req->{item}});
    my $item_barcode   = $item->{barcode} || fail ERR_ELEMENT_RULE_VIOLATED;
    my $loc = $self->item_home($item_barcode);
    my $patron_barcode = $user->{barcode} || fail ERR_ELEMENT_RULE_VIOLATED;
    my %res;
    my $sip = $self->sip(location => $loc, patron => $patron_barcode);
    %res = $sip->renew($patron_barcode, $item_barcode);
    fail ERR_ITEM_NOT_RENEWABLE if !$res{ok};
    return { item => $item };
}

# --- Helper functions

sub find_user {
    my ($self, %user) = @_;
    my ($uid, $barcode) = @user{qw(id barcode)};
    my ($last, $first, $name, $pin, %address);
    # Fill out user information using data from the Voyager database
    if (defined $uid) {
        if ($uid =~ /^\d+$/) {
            ($last, $first, $barcode, $pin) = $self->query1($sql_patron_by_id, $uid);
            if (!defined $barcode) {
                ($uid, $last, $first, $barcode, $pin) = $self->query1($sql_patron_by_non_numeric_id, $uid);
                return if !defined $uid;
            }
            $user{barcode} = $barcode;
        }
        else {
            $uid = uc $uid;
            $uid =~ tr/A-Z0-9//cd;
            ($uid, $last, $first, $barcode, $pin) = $self->query1($sql_patron_by_non_numeric_id, $uid);
            return if !defined $uid;
            $user{barcode} = $barcode;
        }
    }
    elsif ($barcode) {
        $barcode =~ tr/-//d;
        ($last, $first, $uid) = $self->query1($sql_patron_by_barcode, $barcode);
        $user{id} = $uid;
    }
    else {
        return;
    }
    $name = join(' ', grep { defined } ($first, $last));
    @user{qw(last first name)} = ($last, $first, $name);
    @address{qw(line1 city state zip phone)} = $self->query1($sql_patron_address, $uid);
    my ($email) = $self->query1($sql_patron_email, $uid);
    $user{phone} = delete $address{phone};
    $user{address} = \%address;
    $user{email} = $email if defined $email;
    return \%user;
}

sub validate_user {
    my ($self, %user) = @_;
    my ($uid, $barcode, $pin) = $user{qw(id barcode pin)};
    return 0, "can't validate without PIN" if !defined $uid && !defined $pin;
    %user = %{ $self->find_user(%user) || return 0, "can\'t find user" };
    $uid ||= $user{uid};
    $barcode ||= $user{barcode};
    my ($stored_pin, $last) = $user{qw(pin last)};
    return 0, "can't validate without barcode and PIN"
        if !defined $uid && ( !$barcode || !defined $pin || !length $pin );
    if (defined $pin) {
        # Check the PIN that the user provided -- it must match *either*
        # the PIN stored in the Voyager database *or* their last name.
        # Comparisons are case-insensitive.
        $pin = lc $pin;
        $stored_pin = defined $stored_pin ? lc $stored_pin : '';
        $last       = defined $last       ? lc $last       : '';
        return 0, "wrong PIN" if $stored_pin ne $pin && $last ne $pin;
    }
    return \%user;
}

sub find_lendable_holdings_by_bibid {
    my ($self, $agency, $id) = @_;
    my %loc2bibitem;
    my $loc_limit_group = $self->agency_to_location_limit_group($agency);
    my @holdings = $self->queryn($sql_location_limit_group_and_bibid_to_lendable_holdings, $loc_limit_group, $id);
    foreach (@holdings) {
        my ($bib_id, $location, $barcode, $itemid) = @$_;
        push @{ $loc2bibitem{$location} ||= [] }, {
            bib => { id => $bib_id },
            item => { id => $itemid, location => $location, barcode => $barcode },
            location => $location,
        };
    }
    return \%loc2bibitem;
}

sub find_lendable_holdings_by_isbn {
    my ($self, $agency, $isbn) = @_;
    my %loc2bibitem;
    my $loc_limit_group = $self->agency_to_location_limit_group($agency);
    my @holdings = $self->queryn($sql_location_limit_group_and_isbn_to_lendable_holdings, $loc_limit_group, isbn13($isbn));
    foreach (@holdings) {
        my ($bib_id, $location, $barcode, $itemid) = @$_;
        push @{ $loc2bibitem{$location} ||= [] }, {
            bib => { id => $bib_id, isbn => $isbn },
            item => { id => $itemid, location => $location, barcode => $barcode },
            location => $location,
        };
    }
    return \%loc2bibitem;
}

sub agency_to_location_limit_group {
    my ($self, $agency) = @_;
    my $default = $self->{config}{voyager}{default_limit_group} || '<none>';
    return $default if !defined $agency;
    return $self->{config}{agencies}{$agency}{limit_group} || $default;
}

sub score_bibitem {
    # XXX We need a real scorer here; this just picks one at random
    return 100 * rand();
}

sub find_item {
    my ($self, %item) = @_;
    if (!defined $item{id}) {
        return if !defined $item{barcode};
        my ($item_id) = $self->query1($sql_item_barcode_to_id, $item{barcode});
        $item{id} = $item_id || return;
    }
    if (!defined $item{barcode}) {
        $item{barcode} = $self->query1($sql_item_id_to_barcode, $item{id}) || return;
    }
    return \%item;
}

sub find_bibitems {
    my ($self, $agency, %bibitem) = @_;
    my ($id, $barcode, $isbn, $type) = delete @bibitem{qw(id barcode isbn type)};
    # Fill out bib and item information using data from the Voyager database
    my @bibitems;
    if (defined $barcode) {
        my ($b, $l, $i) = $self->query1($sql_item_barcode_to_bib_id_location_and_item_id, $barcode);
        if (defined $b) {
            $bibitem{bib} = { id => $b, isbn => $isbn },
            $bibitem{item} = { id => $i, barcode => $barcode },
            return { $l => [ \%bibitem ] };
        };
    }
    if (defined $id && $type eq REQ_BIBITEM_IS_BIB) {
        my $loc2bibitem = $self->find_lendable_holdings_by_bibid($agency, $id);
        return $loc2bibitem if $loc2bibitem;
    }
    if (defined $isbn) {
        my $loc2bibitem = $self->find_lendable_holdings_by_isbn($agency, $isbn);
        return $loc2bibitem;
    }
    return;
}

sub find_request {
    my ($self, %request) = @_;
    my $id = $request{id} ||= $request{request}{id};
    my $f = $self->request_file($id);
    return $self->read_request($f);
}

sub read_request {
    my ($self, $f) = @_;
    my $req;
    eval { $req = LoadFile($f) };
    return $req;
}

sub write_request {
    my ($self, $f, %data) = @_;
    %data = ( %data, %{$data{request}} ) if $data{request};
    return DumpFile($f, \%data);
}

sub request_file {
    my ($self, $id, $ext) = @_;
    my $root = $self->{config}{files}{root};
    $ext = 'r' if !defined $ext;
    return $ext eq 'r' ? "$root/request/$id.$ext" : "$root/request/cancelled/$id.$ext";
}

sub sip {
    my ($self, %arg) = @_;
    my %config = %{ $self->{config}{sip} };
    my $sip = Biblio::SIP2::Vger8->new(%config, %arg);
    $sip->connect || fail ERR_TEMPORARY_PROCESSING_FAILURE;
    my %res = $sip->login;
    fail ERR_TEMPORARY_PROCESSING_FAILURE if !$res{ok};
    if (defined $arg{patron}) {
        %res = $sip->patron_status($arg{patron});
        fail ERR_UNKNOWN_USER if !$res{ok};
    }
    return $sip;
}


sub item_home {
    my ($self, $item_barcode) = @_;
    my ($loc) = $self->query1($sql_item_home_circ_location, $item_barcode);
    return $loc;
}

sub dbh {
    my ($self) = @_;
    return $dbh ||= Biblio::Vger::DBI->connect(
        database => $self->{config}{voyager}{database} || 'VGER',
    );
}

sub queryn {
    my ($self, $sql) = splice @_, 0, 2;
    my @results;
    my $sth = $sql2sth{$sql} ||= $self->dbh->prepare($sql);
    $sth->execute(@_);
    while (my @row = $sth->fetchrow_array) {
        if (@row > 1) {
            push @results, [ @row ];
        }
        else {
            push @results, $row[0];
        }
    }
    return @results;
}

sub query1 {
    my ($self, $sql) = splice @_, 0, 2;
    my $sth = $sql2sth{$sql} ||= $self->dbh->prepare($sql);
    $sth->execute(@_);
    return $sth->fetchrow_array;
}

sub isbn13 {
    my ($isbn) = @_;
    $isbn =~ tr/-//d;
    return uc $isbn if length($isbn) == 13;
    $isbn = '978' . $isbn;
    # Logic from Business::ISBN13 by brian d foy
    my @digits = split //, $isbn;
    my $sum = 0;
    foreach my $index (0, 2, 4, 6, 8, 10) {
        $sum +=     substr($isbn, $index, 1);
        $sum += 3 * substr($isbn, $index + 1, 1);
    }
    my $checksum = ( 10 * ( int( $sum / 10 ) + 1 ) - $sum ) % 10;
    substr($isbn, -1) = $checksum;
    return $isbn;
}

sub sipdate2iso8601 {
    my ($ts) = @_;
    my ($Y, $m, $d, $H, $M, $S) = (
        substr($ts,  0, 4),
        substr($ts,  4, 2),
        substr($ts,  6, 2),
        substr($ts, 12, 2),
        substr($ts, 14, 2),
        substr($ts, 16, 2),
    );
    $H ||= 0;
    $M ||= 0;
    $S ||= 0;
    sprintf('%04d-%02d-%02dT%02d:%02d:%02d', $Y, $m, $d, $H, $M, $S);
}

sub hook_request_created {
    my ($self, %data) = @_;
    my $id = $data{id} ||= $data{request}{id};
    my $f = $self->request_file($id);
    $self->write_request($f, %data);
    # XXX Log details for notification purposes
}

sub hook_request_cancelled {
    my ($self, %data) = @_;
    my $id = $data{id} ||= $data{request}{id};
    my $fr = $self->request_file($id);
    my $fx = $self->request_file($id, 'x');
    $data{status} = 'cancelled';
    if (rename $fr, $fx) {
        $self->write_request($fx, %data);
    }
    else {
        unlink $fr;
    }
}

sub error($) {
    # In a message handler: return error(ERR_UNKNOWN_USER);
    my ($msg) = @_;
    my $msgtype = caller();
    $msgtype =~ s/.+:://;
    $msgtype .= 'Response';
    return {
        'type'    => $msgtype,
        'message' => $msg,
    };
}

sub fail($) {
    my ($err) = @_;
    $err .= "\n" if !ref $err;
    die $err;
}

1;
