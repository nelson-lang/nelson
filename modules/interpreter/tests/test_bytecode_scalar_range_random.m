%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
previousRng = rng();
cleanupRng = onCleanup(@() rng(previousRng));
rng(9341);
directory = tempname();
mkdir(directory);
cleanupDirectory = onCleanup(@() rmdir(directory, 's'));
addpath(directory);
cleanupPath = onCleanup(@() rmpath(directory));
operators = {'+', '-', '*', '/'};
for sample = 1:40
  body = {};
  for statement = 1:12
    destination = randi(5);
    left = randi(5);
    right = randi(5);
    operation = operators{randi(4)};
    if mod(statement, 4) == 0
      expression = sprintf('-(r%d %s r%d) / 2', left, operation, right);
    elseif mod(statement, 4) == 1
      expression = sprintf('r%d %s 0.5', left, operation);
    else
      expression = sprintf('r%d %s r%d', left, operation, right);
    end
    body{end + 1} = sprintf('r%d = %s;', destination, expression);
  end
  forName = sprintf('scalar_range_generated_for_%d', sample);
  whileName = sprintf('scalar_range_generated_while_%d', sample);
  initial = {'r1 = 1; r2 = 2; r3 = -3; r4 = 0.5; r5 = 4;'};
  result = {'out = [r1, r2, r3, r4, r5];', 'end'};
  forLines = [{['function out = ', forName, '()']}, initial, ...
    {'for k = 1:35'}, body, {'end'}, result];
  whileLines = [{['function out = ', whileName, '()']}, initial, ...
    {'k = 1;', 'while k <= 35'}, body, {'k = k + 1;', 'end'}, result];
  filewrite([directory, '/', forName, '.m'], strjoin(forLines, sprintf('\n')));
  filewrite([directory, '/', whileName, '.m'], strjoin(whileLines, sprintf('\n')));
  rehash();
  asserts.istrue(isequaln(feval(forName), feval(whileName)));
end
