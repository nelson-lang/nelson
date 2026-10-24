%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: parse cost of a pure script vs. a script that interleaves local
% function definitions with top-level statements. The mixed-script case is
% routed straight to the mixed parser (no doomed grammar parse + exception),
% so its overhead over a plain script stays modest.
%=============================================================================
R = 20000;
%=============================================================================
pure_script = sprintf(['a = 1;\n', ...
  'b = 2;\n', ...
  'c = a + b;\n', ...
  'd = c * 2;\n', ...
  'e = d - 1;\n']);
%=============================================================================
function_file = sprintf(['function y = bench_helper_one(x)\n', ...
  '  y = x + 1;\n', ...
  'end\n', ...
  'function y = bench_helper_two(x)\n', ...
  '  y = x * 2;\n', ...
  'end\n']);
%=============================================================================
mixed_script = sprintf(['a = 1;\n', ...
  'b = 2;\n', ...
  'c = benchHelperOne(a);\n', ...
  'function y = benchHelperOne(x)\n', ...
  '  y = x + 1;\n', ...
  'end\n', ...
  'd = benchHelperTwo(b);\n', ...
  'function y = benchHelperTwo(x)\n', ...
  '  y = x * 2;\n', ...
  'end\n', ...
  'e = c + d;\n']);
%=============================================================================
% warmup
parsestring(pure_script);
parsestring(function_file);
parsestring(mixed_script);
%=============================================================================
tic();
for i = 1:R
  parsestring(pure_script);
end
t_pure = toc();
fprintf('  pure script parse:      %0.2f us/parse\n', t_pure / R * 1e6);
%=============================================================================
tic();
for i = 1:R
  parsestring(function_file);
end
t_func = toc();
fprintf('  function file parse:    %0.2f us/parse\n', t_func / R * 1e6);
%=============================================================================
tic();
for i = 1:R
  parsestring(mixed_script);
end
t_mixed = toc();
fprintf('  mixed script parse:     %0.2f us/parse\n', t_mixed / R * 1e6);
fprintf('  mixed / pure ratio:     %0.1fx\n', t_mixed / t_pure);
%=============================================================================
