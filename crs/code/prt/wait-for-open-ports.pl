#!/usr/bin/env perl
# Treat every arg as a port number that must be open before test can run.



my @required_ports = @ARGV;
print "Required ports: " . join("|", @required_ports) . "\n";

my $max_attempts = 10;
my $secs_between_attempts = 1;

my $attempts = 0;

my @active_ports = active_ports();
print "Active ports: " . join("|", @active_ports) . "\n";

#my @intersection = intersection_string_equality(\@required_ports, \@active_ports);
#print "Intersection: ". join("|", @intersection) . "\n";

######################################################################
# main loop
######################################################################

while ($attempts < $max_attempts) {
    $attempts++;
    my @active_ports = active_ports();
    my @active_required_ports = intersection_string_equality(\@required_ports, \@active_ports);
    if (0 ==  scalar @active_required_ports) {
        print "[wait-for-open-ports.pl] All required ports are available.\n";
        ## EXIT EXIT EXIT
        exit(0)
    } else {
        print "[wait-for-open-ports.pl] On attempt $attempts, required ports are active: " . join(", ", @active_required_ports) . "\n";
        sleep($secs_between_attempts);
    }
}

# We ran out of attempts!
print "[wait-for-open-ports.pl] All required ports are NOT available after $max_attempts attempts.\n";
exit(1);

sub active_ports {
    #   TODO This prints some useless info to stderr.
    my @ports;
    my @output = `netstat -ntpl`;
    die "Couldn't run netstat" unless @output;
    
    while (my $line = shift @output){
        chomp $line;
        if (my ($recvq,$sendq,$local_addr,$local_port) = 
            $line=~/^tcp\s+
                (\d+)\s+
                (\d+)\s+
                (\d+\.\d+\.\d+.\d+:(\d+)\s+)/x) {
            #print "$local_port\n";
            push @ports, $local_port;
        }
    }
    return @ports;
}

sub intersection_string_equality {
    my ($array_ref_1, $array_ref_2) = @_;
    my @result;
    my ($val_1, $val_2);
    foreach $val_1 (@$array_ref_1) {
        foreach $val_2 (@$array_ref_2) {
            #print "test: $val_1 eq $val_2\n";
            if ($val_1 eq $val_2) {
                push @result,$val_1;
                last;
            }
        }
    }
    return @result;
}
