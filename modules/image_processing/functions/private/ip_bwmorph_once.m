%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_bwmorph_once(bw, operation)
  neighbors = conv2(double(bw), ones(3, 3), 'same') - double(bw);
  switch operation
    case 'clean'
      out = bw & neighbors > 0;
    case 'fill'
      out = bw | (~bw & neighbors == 8);
    case 'majority'
      out = conv2(double(bw), ones(3, 3), 'same') >= 5;
    case 'remove'
      out = bw & neighbors < 8;
    case 'endpoints'
      out = bw & neighbors == 1;
    case 'branchpoints'
      out = bw & neighbors >= 3;
    case 'spur'
      out = bw & neighbors ~= 1;
    case 'bridge'
      out = ip_bwmorph_bridge(bw);
    otherwise
      error('Nelson:image_processing:unsupportedBwmorphOperation', 'Unsupported "%s" option.', 'bwmorph operation');
  end
end
%=============================================================================
