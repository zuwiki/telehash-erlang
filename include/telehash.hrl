%% switch
% Holds the current state for a Switch
% ipp: This Switch's external IP:PORT in the form {IP={_,_,_,_}, PortInt}
%       Bootstrapping is required if this is undefined
% hash: SHA1(ipp) - this Switch's location in the global DHT; should be a fun?
% socket: The switch's UDP socket. Might be a bad idea storing it.
% lines: A list of active lines to other switches, with their taps. See below.
% buckets: Four lists of Ends sorted according to their proximity. See below.
% callbacks: An a-list of taps (for this Switch) with funs to call(?)
-record(switch,   {ipp,
                   hash,
                   socket,
                   lines=[],
                   buckets={[],[],[],[]},
                   callbacks=[]}).

-record(tap, {rules=[]}).

-record(telex, {dict}).