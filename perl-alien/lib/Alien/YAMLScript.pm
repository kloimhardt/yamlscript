use strict;
use warnings;

package Alien::YAMLScript;

our $VERSION = '0.1.60';

use parent 'Alien::Base';

our $libyamlscript_version = '0.1.60';

die "Alien::YAMLScript $VERSION requires libyamlscript $libyamlscript_version" .
    "but you have " . __PACKAGE__->version
    unless Alien::YAMLScript->exact_version($libyamlscript_version);

1;
