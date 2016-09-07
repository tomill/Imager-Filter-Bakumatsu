use inc::Module::Install;

name 'Imager-Filter-Bakumatsu';
all_from 'lib/Imager/Filter/Bakumatsu.pm';

test_requires 'Test::More';
test_requires 'File::Compare';
test_requires 'File::Temp';

requires 'Imager';
requires 'File::ShareDir' => 1.00;

install_share 'share';
auto_include;

WriteAll;
