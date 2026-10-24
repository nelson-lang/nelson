%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
asserts.isequal(arguments_edge_nv_cross_reference(), 2);
%=============================================================================
R = arguments_edge_nv_struct_defaults();
asserts.isequal(R{1}, 'linear');
asserts.isequal(R{2}, 2);
asserts.isequal(R{3}, 5);
%=============================================================================
opts = [];
opts.Scale = 4;
R = arguments_edge_nv_struct_provided(opts);
asserts.isequal(R{1}, 4);
asserts.isequal(R{2}, 5);
%=============================================================================
asserts.isequal(arguments_edge_nv_nested_struct(), 7);
%=============================================================================
M = [];
try
  opts = [];
  opts.Scale = -1;
  arguments_edge_nv_struct_provided(opts);
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, 'positive'));
%=============================================================================
