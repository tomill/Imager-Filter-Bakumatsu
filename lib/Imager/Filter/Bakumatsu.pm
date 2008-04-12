package Imager::Filter::Bakumatsu;
use strict;
use warnings;
our $VERSION = '0.01';

use Imager;
use File::ShareDir 'module_file';

Imager->register_filter(
    type     => 'bakumatsu',
    callsub  => \&bakumatsunize,
    callseq  => [],
    defaults => {
        overlay_image => module_file(__PACKAGE__, 'BakumatsuTexture.png'),
    },
);

sub bakumatsunize {
    my %opt  = @_;
    my $self = delete $opt{imager};
    my $work  = $self;
    
    $work = $work->convert(
        matrix => [
            [ 1.7,   0,   0, 0 ], 
            [   0, 1.7    0, 0 ], 
            [   0,   0, 1.7, 0 ], 
        ],
    ); 
    
    $work->filter(
        type      => 'contrast', 
        intensity => 1.2,
    ); 
    
    $work->filter(
        type => 'autolevels', 
        lsat => 0.3,
        usat => 0.3,
    );
    
    $work = $work->convert(
        matrix => [
            [ 1 / 4, 1 / 2, 1 / 8, 0 ], 
            [ 1 / 4, 1 / 2, 1 / 8, 0 ], 
            [ 1 / 4, 1 / 2, 1 / 8, 0 ], 
        ],
    ); 
    
    $work->rubthrough(
        src => do {
            my $overlay = Imager->new;
               $overlay->read(file => $opt{overlay_image})
                or die $overlay->errstr;
            
            $overlay = $overlay->scale(
                xpixels => $work->getwidth,
                ypixels => $work->getheight,
                type    => 'nonprop'
            );
        },
    ) or die $work->errstr;
    
    $self->{IMG} = delete $work->{IMG};
}

1;
__END__

=encoding utf-8

=head1 NAME

Imager::Filter::Bakumatsu -

=head1 SYNOPSIS
  
  use Imager;
  use Imager::Filter::Bakumatsu;
  
  my $img = Imager->new;
  $img->read(file => 'photo.jpg');
  
  $img->filter(type => 'bakumatsu');
  
  $img->write(file => 'photo-bakumatsu.png');

=head1 DESCRIPTION

Imager::Filter::Bakumatsu is

=head1 AUTHOR

Naoki Tomita E<lt>tomita@cpan.orgE<gt>

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

Sample form: L<http://bakumatsu.koneta.org/>

Original idea: L<http://labs.wanokoto.jp/olds>,
L<http://d.hatena.ne.jp/nitoyon/20080407/bakumatsu_hack>

=cut
