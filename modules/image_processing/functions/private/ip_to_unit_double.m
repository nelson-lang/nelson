%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = ip_to_unit_double(img)
  switch class(img)
    case 'double'
      d = img;
    case 'single'
      d = double(img);
    case 'logical'
      d = double(img);
    case 'uint8'
      d = double(img) / 255;
    case 'uint16'
      d = double(img) / 65535;
    case 'uint32'
      d = double(img) / 4294967295;
    case 'uint64'
      d = double(img) / 18446744073709551615;
    case 'int8'
      d = (double(img) + 128) / 255;
    case 'int16'
      d = (double(img) + 32768) / 65535;
    case 'int32'
      d = (double(img) + 2147483648) / 4294967295;
    case 'int64'
      d = (double(img) + 9223372036854775808) / 18446744073709551615;
    otherwise
      d = double(img);
  end
end
%=============================================================================
