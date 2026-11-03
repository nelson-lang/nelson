%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Pool workers parse anonymous functions and load function files at the same time.
%=============================================================================
% the test runner starts with one thread: the pool needs several workers before it is created
nbWorkers = 4;
previousThreads = maxNumCompThreads(nbWorkers);
threadsCleanup = onCleanup(@() maxNumCompThreads(previousThreads)); %#ok<NLS0004>
asserts.isequal(backgroundPool().NumWorkers, nbWorkers);
nbHandles = 1500;
parseOne = @(k, w) feval(str2func(sprintf('@(x) x + %d * (x - %d)', k + w, k)), 2);
parseHandles = @(n, w) sum(cellfun(@(k) parseOne(k, w), num2cell(1:n)));
for round = 1:3
  for w = 1:nbWorkers
    F(w) = parfeval(backgroundPool, parseHandles, 1, nbHandles, w); %#ok<NLS0039>
  end
  R = fetchOutputs(F);
  for w = 1:nbWorkers
    k = 1:nbHandles;
    asserts.isequal(R(w), sum(2 + (k + w) .* (2 - k)));
  end
end
%=============================================================================
% every worker runs the same function files in the same order: they share each definition
LF = char(10);
nbFunctions = 40;
functionsDirectory = tempname();
mkdir(functionsDirectory);
directoryCleanup = onCleanup(@() rmdir(functionsDirectory, 's')); %#ok<NLS0004>
names = cell(1, nbFunctions);
for k = 1:nbFunctions
  names{k} = sprintf('concurrent_parse_fun_%d', k);
  filewrite(fullfile(functionsDirectory, [names{k}, '.m']), ...
    ['function r = ', names{k}, '(x)', LF, sprintf('  r = x * %d + numel([1, 2, 3]);', k), LF, 'end', LF]);
end
addpath(functionsDirectory);
pathCleanup = onCleanup(@() rmpath(functionsDirectory)); %#ok<NLS0004>
for w = 1:nbWorkers
  F(w) = parfeval(backgroundPool, @(c) sum(cellfun(@(name) feval(name, 2), c)), 1, names);
end
R = fetchOutputs(F);
asserts.isequal(R, repmat(sum(2 * (1:nbFunctions) + 3), nbWorkers, 1));
%=============================================================================
clear pathCleanup
clear directoryCleanup
clear threadsCleanup
%=============================================================================
