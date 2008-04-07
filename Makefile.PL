use inc::Module::Install;
name 'Imager-Filter-Bakumatsu';
all_from 'lib/Imager/Filter/Bakumatsu.pm';

build_requires 'Test::More';
use_test_base;
install_share;
auto_include;
WriteAll;
