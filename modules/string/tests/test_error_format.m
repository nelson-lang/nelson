%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% error() with an identifier and a format that has more specs than data keeps
% the identifier and formats the available part; the internal "No more data."
% message must never leak.
try
  error('a:b', 'no args %d %s');
  R = 'no error';
catch e
  R = e;
end
asserts.isequal(R.identifier, 'a:b');
asserts.isequal(R.message, 'no args ');
%=============================================================================
