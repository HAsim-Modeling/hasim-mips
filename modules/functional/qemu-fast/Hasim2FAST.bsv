import hasim_common::*;

import fast_channelio::*;
import fast_funcp::*;

module [HASim_Module] mkFUNCP();
    let fastfuncp <- mkIfc;
    let fastchannelio <- mkFastChannelIO (fastfuncp);
endmodule
