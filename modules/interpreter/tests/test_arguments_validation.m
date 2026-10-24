%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
addpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
R = arguments_validation_function([1, 2, 3]);
asserts.isequal(R{1}, [1, 2, 3]);
asserts.isequal(R{2}, 'linear');
%=============================================================================
R = arguments_validation_function([1, 2, 3], 'nearest');
asserts.isequal(R{2}, 'nearest');
%=============================================================================
R = arguments_validation_function([1; 2; 3]);
REF = {[1, 2, 3], 'linear'};
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('arguments_validation_function([1, 2, 3], ''cubic'')', _('Value must be member of the compared value.'));
%=============================================================================
R = arguments_output_block_function("1", 3, {1, 2; 3, 4});
asserts.isequal(R, 3);
%=============================================================================
asserts.checkerror('arguments_output_block_function("1", 3)', _('Not enough input arguments.'));
%=============================================================================
msg = ['Invalid output ''out''. ', _('Value must have one of these types:'), ' ''double''.'];
asserts.checkerror('R = arguments_output_invalid_function();', msg);
%=============================================================================
R = arguments_order_function(1.8, 1.5);
asserts.isequal(R, uint32(4));
asserts.isequal(class(R), 'uint32');
%=============================================================================
R = arguments_order_function(1.8, 1.5, 25);
asserts.isequal(R, uint32(25));
asserts.isequal(class(R), 'uint32');
%=============================================================================
R = arguments_edge_out_convertible();
asserts.isequal(R, 3);
asserts.isequal(class(R), 'double');
%=============================================================================
R = arguments_edge_out_scalar_row();
asserts.isequal(R, 3);
asserts.isequal(size(R), [1, 1]);
%=============================================================================
R = arguments_edge_out_column_row();
asserts.isequal(R, [1, 2, 3]);
asserts.isequal(size(R), [1, 3]);
%=============================================================================
M = [];
try
  arguments_edge_out_fixed_mismatch();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, 'Invalid output ''out''.'));
asserts.istrue(contains(M.message, _('Value must match declared argument size.')));
%=============================================================================
M = [];
try
  arguments_edge_out_empty_row();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, 'Invalid output ''out''.'));
asserts.istrue(contains(M.message, _('Value must be a row vector.')));
%=============================================================================
M = [];
try
  arguments_edge_out_unconvertible();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, 'Invalid output ''out''.'));
%=============================================================================
asserts.isequal(arguments_edge_in_char_string('abc'), 'string');
asserts.isequal(arguments_edge_in_single_double(single([1, 2])), 'double');
%=============================================================================
R = arguments_edge_in_column_to_row([1; 2; 3]);
asserts.isequal(R, [1, 2, 3]);
asserts.isequal(size(R), [1, 3]);
%=============================================================================
R = arguments_bytecode_indexing([10; 20; 30], 2);
asserts.isequal(R, 20);
%=============================================================================
R = arguments_edge_in_scalar_expansion(5);
asserts.isequal(R, [5, 5, 5]);
%=============================================================================
R = arguments_edge_in_invalid_default(7);
asserts.isequal(R, 7);
R = arguments_edge_in_invalid_default();
REF = [110 111 116 95 97 95 100 111 117 98 108 101];
asserts.isequal(R, REF);
%=============================================================================
M = [];
try
  arguments_edge_in_forward_default();
catch
  M = lasterror();
end
asserts.istrue(~isempty(M.message));
%=============================================================================
R = arguments_edge_in_backward_default(1.8);
asserts.isequal(R, uint32(4));
asserts.isequal(class(R), 'uint32');
%=============================================================================
asserts.isequal(arguments_edge_val_converted(int32(5)), 'double');
M = [];
try
  arguments_edge_val_converted(int8(-200));
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, 'positive'));
%=============================================================================
asserts.isequal(arguments_edge_nv_cross_reference(), 2);
%=============================================================================
M = [];
try
  arguments_edge_misc_nargin_default();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, 'nargin'));
%=============================================================================
M = [];
try
  arguments_edge_misc_output_before_input(1);
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, _('arguments (Input) block must appear before arguments (Output) block.')));
%=============================================================================
