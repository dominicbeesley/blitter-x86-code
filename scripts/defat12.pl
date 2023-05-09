#!/usr/bin/env perl

use strict;
use Fcntl qw(:seek);
use POSIX qw(ceil);

my $fn_in = shift;

open(my $fh_in, "<:raw", $fn_in) or die "Cannot open '$fn_in' for input: $!";

my $sec_boot;
read($fh_in, $sec_boot, 512) == 512 or die "Cannot read boot sector";

my (
	$bpsec, $secpclu, $ressec, $FATno, 
	$rootents, $totsec, $mediabyte, $secsperfat, 
	$secspertrack, $heads, $hiddensecs, $totsecs64, 
	$driveno, $resv0, $extbootbyte, $serialno, 
	$vollbl, $fstype) = 
	unpack("S C S C S S C S S S L L C C C L A11 A8", substr($sec_boot, 0x0b, 60));

printf "bytes per sec\t%d\n",$bpsec;
printf "secs per clus\t%d\n",$secpclu;
printf "resv.d secs\t%d\n",$ressec;
printf "# FAT copies\t%d\n",$FATno;


printf "# root ents\t%d\n",$rootents;
printf "tot secs\t%d\n",$totsec;
printf "media byte\t%02X\n",$mediabyte;
printf "secs per fat\t%d\n",$secsperfat;

printf "secs per track\t%d\n",$secspertrack;
printf "heads\t\t%d\n",$heads;
printf "hidden\t\t%d\n",$hiddensecs;
printf "totsecs64\t%d\n",$totsecs64;

printf "drive #\t\t%d\n",$driveno;
printf "reserved\t%d\n",$resv0;
printf "extboot byte\t%02X\n",$extbootbyte;
printf "serial no\t%04X\n",$serialno;

printf "vol lbl\t\t%s\n",$vollbl;
printf "fs type\t\t%s\n",$fstype;


# skip reserved sectors
seek($fh_in, $bpsec*$ressec, SEEK_SET);

my @FAT = ();
for (my $i = 0; $i < $FATno; $i++) {
	read($fh_in, @FAT[$i], $bpsec*$secsperfat) == $bpsec*$secsperfat or die "EOF reading FAT[$i]";
}

if ($FATno > 1) {
	for (my $i = 1; $i < $FATno; $i++) {
		if (@FAT[0] ne @FAT[$i]) {
			print "*** WARNING FATS 0 and $i do not match!\n";
		}
	}
}

my @FATd = ();	#decoded 12 bit fats
foreach my $te (unpack("(a3)*", @FAT[0])) {
	my @ccc = unpack("C3", $te);
	my $a = @ccc[0] + ((@ccc[1] & 0x0f) << 8);
	my $b = ((@ccc[1] & 0xf0) >> 4) + (@ccc[2] << 4) ;
	push @FATd, $a;
	push @FATd, $b;
}

my @clusters = ();
my $i = 0;
my $j = 0;
my $had0 = 0;
while ($i < scalar @FATd) {
	my @cl = ();
	while ($i < scalar @FATd) {
		my $lba = @FATd[$i++];
		if ($i != 1) {
			if ($lba == 0) {
				scalar @cl == 0 or print "Warning cluster #$j ended with 000\n";
				$had0 = 1;
				last;
			} elsif ($lba == 0xFF6) {
				print "Warning reserved FF6 in cluster $j\n";
			} elsif ($lba == 0xFF7) {
				print "Bad sector FFF7 in cluster $j\n";
			} elsif ($lba >= 0xFF0 ) {			
				last;
			}
		}
		push @cl, $lba;
	}
	if (scalar @cl) {
		print ".";
		if ($had0) {
			print "Warning blank cluster before valid cluster\n";
		}
		@clusters[$j++] = \@cl;
	}
}

my $j = 0;
foreach my $cl (@clusters) {
	printf "%d:", $j++;
	$i = 0;
	do {
		if ($i != 0) {
			print ",";
		}
		my $ix = $cl->[$i];
		my $ix2 = $ix;
		$i++;
		while ($i < scalar @$cl && $cl->[$i] == $ix2 + 1) {
			$i++;
			$ix2++;
		}
		printf "#%02X", $ix;
		if ($ix2 > $ix) {
			printf "-#%02X", $ix2;
		}

	} while ($i < scalar @$cl);
	print "\n";
}


# this should be the root director build a directory structure
my $rootdirbytes = 32 * $rootents;
my $rootsecs = ceil($rootdirbytes/$bpsec);
my $rootdir;

my @dir_root = ();

read ($fh_in, $rootdir, $bpsec*$rootsecs) == $bpsec*$rootsecs or die "Unexpected EOF in root dir";

#TODO: don't ignore deleted/zero'd entries
foreach my $de (unpack("(a32)*", $rootdir)) {
	my ($nam,$ext,$att,$mod,$clu,$len) = unpack("a8 a3 C x10 L S L", $de);

	my $nam0 = unpack("C", $nam);
	if ($nam0 != 0x00 && $nam0 != 0xE5) {
		if ($nam0 == 0x06) {
			$nam = chr(0xE5) . substr($nam,2);
		}
		printf "%8s.%3s %02X %08X %03x %6d => ", $nam, $ext, $att, $mod, $clu, $len;

		my $lba = clu2lba($clu);
		my ($c, $h, $s) = lba2chs($lba);
		printf "A=%08X, L=%08X, CHS=%02X,%1X,%1X\n", $bpsec*$lba, $lba, $c, $h, $s;
	}
}


sub clu2lba($) {
	my ($clu) = @_;
	return (($clu - 2) * $secpclu) + 1 + ($FATno*$secsperfat) + $rootsecs;
}

sub lba2chs($) {
	my ($lba) = @_;
	my $sec = 1 + ($lba % $secspertrack);
	$lba = $lba / $secspertrack;
	my $head = $lba % $heads;
	$lba = $lba / $heads;
	my $cyl = $lba;
	return ($cyl,$head,$sec);
}