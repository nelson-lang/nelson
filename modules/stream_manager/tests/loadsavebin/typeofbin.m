%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = typeofbin(code_type)
  switch(code_type)
    case 201
      res = 'sparsedouble';
    case 204
      res = 'sparselogical';
    case 104
      res = 'ndarraylogical';
    case 181
      res = 'ndarrayuint8';
    case 182
      res = 'ndarrayint8';
    case 183
      res = 'ndarrayuint16';
    case 184
      res = 'ndarrayint16';
    case 185
      res = 'ndarrayuint32';
    case 186
      res = 'ndarrayint32';
    case 187
      res = 'ndarrayuint64';
    case 188
      res = 'ndarrayint64';
    case 102
      res = 'ndarraysingle';
    case 101
      res = 'ndarraydouble';
    case 110
      res = 'ndarraystring';
    case 117
      res = 'ndarraycell';
    case 115
      res = 'ndarraystruct';
    case 18
      res = 'string';
    case 17
      res = 'cell';
    case 15
      res = 'struct';
    case 4
      res = 'logical';
    case 81
      res = 'uint8';
    case 82
      res = 'int8';
    case 83
      res = 'uint16';
    case 84
      res = 'int16';
    case 85
      res = 'uint32';
    case 86
      res = 'int32';
    case 87
      res = 'uint64';
    case 88
      res = 'int64';
    case 2
      res = 'single';
    case 1
      res = 'double';
    case 10
      res = 'char';
    case 14
      res = 'function_handle';
    case 9
      res = 'handle';
    case 1000
      res = 'generic';
    case 2000
      res = 'class';
    end
  end
  
