%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname(tempdir());
mkdir(folder);
cleanup = onCleanup(@() rmdir(folder, 's'));
raise = '  error(''fixture:catch'', ''boom'');';
namedBody = 'result = numel(e.message);';
cases = {};
cases{end + 1} = {'try', raise, ['catch e, ', namedBody], 'end', 'result = result + 1;'};
cases{end + 1} = {'try', raise, ['catch e; ', namedBody], 'end', 'result = result + 1;'};
cases{end + 1} = {'try', raise, 'catch, result = 4;', 'end', 'result = result + 1;'};
cases{end + 1} = {'try', raise, 'catch; result = 4;', 'end', 'result = result + 1;'};
cases{end + 1} = {'try', raise, 'catch e', ['  ', namedBody], 'end', 'result = result + 1;'};
cases{end + 1} = {'try', raise, 'catch', '  result = 4;', 'end', 'result = result + 1;'};
cases{end + 1} = {['try, ', strtrim(raise), ' catch e, ', namedBody, ' end'], ...
  'result = result + 1;'};
cases{end + 1} = {['try; ', strtrim(raise), ' catch e; ', namedBody, ' end'], ...
  'result = result + 1;'};
cases{end + 1} = {['try, ', strtrim(raise), ' catch, result = 4; end'], 'result = result + 1;'};
cases{end + 1} = {['try; ', strtrim(raise), ' catch; result = 4; end'], 'result = result + 1;'};
cases{end + 1} = {['try, ', strtrim(raise), ' catch e, end'], 'result = numel(e.message) + 1;'};
cases{end + 1} = {'try', raise, 'catch e, % retained separator', ['  ', namedBody], ...
  'end', 'result = result + 1;'};

% Execute every source before the first formatting call; invalid fixtures must fail here.
files = cell(size(cases));
for k = 1:numel(cases)
  files{k} = fullfile(folder, ['catch_', int2str(k), '.m']);
  filewrite(files{k}, strjoin(strtrim(cases{k}), char(10)));
  executeCatchFixture(files{k});
end
for k = 1:numel(cases)
  expected = strjoin(cases{k}, char(10));
  % Both layouts must preserve the executable catch binding and the source separator.
  for fullFormat = [false, true]
    smartindent(files{k}, 2, false, fullFormat);
    asserts.isequal(normalizedCatchText(files{k}), expected);
    executeCatchFixture(files{k});
    smartindent(files{k}, 2, false, fullFormat);
    asserts.isequal(normalizedCatchText(files{k}), expected);
    filewrite(files{k}, strrep(expected, char(10), char([13, 10])));
  end
end

function executeCatchFixture(filename)
  output = evalc('run(filename);');
  asserts.isequal(result, 5);
end

function text = normalizedCatchText(filename)
  text = strrep(fileread(filename, 'char', 'native'), char([13, 10]), char(10));
end
