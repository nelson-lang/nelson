%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
script_cases = {};
script_cases{end + 1} = 'a = [1 2; 3 4];';
script_cases{end + 1} = 'b = {''a'', "b"};';
script_cases{end + 1} = 'c = 3.'' + 4.'';';
script_cases{end + 1} = 'd = f(x = 1, y = 2);';
script_cases{end + 1} = sprintf('e = [1 ...\n 2];');
script_cases{end + 1} = sprintf('%% comment only line\nf = 1;');
script_cases{end + 1} = '[~, g] = size(ones(2, 3));';
%=============================================================================
function_case = sprintf(['function y = parser_context_isolation_f(x)\n', ...
  'arguments\n', ...
  '  x double\n', ...
  'end\n', ...
  'y = x.'';\n', ...
  'end']);
%=============================================================================
for k = 1:40
  assert_isequal(parsestring(script_cases{mod(k - 1, numel(script_cases)) + 1}), 'script');
  assert_isequal(parsestring(function_case), 'function');
  assert_isequal(parsestring('function y = broken_function(x)'), 'error');
  assert_isequal(parsestring('z = 1;'), 'script');
end
%=============================================================================
