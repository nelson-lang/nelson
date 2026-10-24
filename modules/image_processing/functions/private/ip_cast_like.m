%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_cast_like(values, model)
  switch class(model)
    case 'uint8'
      out = uint8(min(max(round(values), 0), 255));
    case 'uint16'
      out = uint16(min(max(round(values), 0), 65535));
    case 'uint32'
      out = uint32(min(max(round(values), 0), 4294967295));
    case 'uint64'
      out = uint64(min(max(round(values), 0), double(intmax('uint64'))));
    case 'int8'
      out = int8(min(max(round(values), -128), 127));
    case 'int16'
      out = int16(min(max(round(values), -32768), 32767));
    case 'int32'
      out = int32(min(max(round(values), -2147483648), 2147483647));
    case 'int64'
      out = int64(min(max(round(values), double(intmin('int64'))), double(intmax('int64'))));
    case 'logical'
      out = logical(values);
    case 'single'
      out = single(values);
    otherwise
      out = values;
  end
end
%=============================================================================
