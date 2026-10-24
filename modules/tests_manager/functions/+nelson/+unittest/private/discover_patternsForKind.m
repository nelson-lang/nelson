%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function patterns = discover_patternsForKind(kind) %#ok<NLS0026>
  switch kind %#ok<NLS0008>
    case 'test'
      patterns = {'test_*.m'};
    case 'bug'
      patterns = {'bug_*.m'};
    case 'bench'
      patterns = {'bench_*.m'};
    case {'all_tests', 'tests'}
      patterns = {'test_*.m', 'bug_*.m'};
    case 'all'
      patterns = {'test_*.m', 'bug_*.m', 'bench_*.m'};
    otherwise
      error('Nelson:tests_manager:unsupportedTestKind', 'Unsupported test kind.');
  end
end
