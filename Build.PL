use strict;
use warnings;

use Module::Build;

my %module_build_args = (
    'build_requires' => {
        'Module::Build' => 0,
        'Test::More' => 0
    },
    'configure_requires' => {
        'Module::Build' => 0,
    },
    'dist_abstract' => 'Voyager 8 back-end for NCIP version 1',
    'dist_author' => [ 'Paul Hoffman <paul@flo.org>' ],
    'dist_name' => 'Biblio-NCIP1-Backend-Vger8',
    #'dist_version' => '0.03',
    'license' => 'perl',
    'module_name' => 'Biblio::NCIP1::Backend::Vger8',
    'recommends' => {},
    'recursive_test_files' => 1,
    'requires' => {
        'Biblio::NCIP1' => 0,
        'Biblio::SIP2::Vger8' => 0,
        'Biblio::Vger::DBI' => 0,
        'Carp' => 0,
        'Package::Constants' => 0,
        'XML::Simple' => 0,
        'YAML' => 0,
    },
    'script_files' => [],
);

my $build = Module::Build->new(%module_build_args);

$build->create_build_script;
