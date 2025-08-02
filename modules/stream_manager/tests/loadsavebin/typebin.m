%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = typebin(strtype)
  switch(strtype)
    case 'sparsedouble'
      res = 201;
    case 'sparselogical'
      res = 204;
    case 'ndarraylogical'
      res = 104;
    case 'ndarrayuint8'
      res = 181;
    case 'ndarrayint8'
      res = 182;
    case 'ndarrayuint16'
      res = 183;
    case 'ndarrayint16'
      res = 184;
    case 'ndarrayuint32'
      res = 185;
    case 'ndarrayint32'
      res = 186;
    case 'ndarrayuint64'
      res = 187;
    case 'ndarrayint64'
      res = 188;
    case 'ndarraysingle'
      res = 102;
    case 'ndarraydouble'
      res = 101;
    case 'ndarraychar'
      res = 110;
    case 'ndarraycell'
      res = 117;
    case 'ndarraystruct'
      res = 115;
    case 'string'
      res = 18;
    case 'cell'
      res = 17;
    case 'struct'
      res = 15;
    case 'logical'
      res = 4;
    case 'uint8'
      res = 81;
    case 'int8'
      res = 82;
    case 'uint16'
      res = 83;
    case 'int16'
      res = 84;
    case 'uint32'
      res = 85;
    case 'int32'
      res = 86;
    case 'uint64'
      res = 87;
    case 'int64'
      res = 88;
    case 'single'
      res = 2;
    case 'double'
      res = 1;
    case 'char'
      res = 10;
    case 'function_handle'
      res = 14;
    case 'handle'
      res = 9;
    case 'generic'
      res = 1000;
    otherwise
      res = -1;
    end
  end
  
