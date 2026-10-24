%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function writeSuite(fp, test_suite, eol)
  fprintf(fp, ['  <testsuite name="', test_suite.name, '" id="', test_suite.id, ...
    '" time="', mat2str(test_suite.time, 2), '" tests="', int2str(test_suite.tests), ...
    '" skipped="', int2str(test_suite.disabled), '" errors="', int2str(test_suite.errors), ...
    '" hostname="', test_suite.hostname, '" timestamp="', test_suite.timestamp, '" ', '>', eol]);
  fprintf(fp, ['    <properties>', eol]);
  fprintf(fp, ['      <property name="', 'environment', '" value="', test_suite.environment, '"/>', eol]);
  fprintf(fp, ['    </properties>', eol]);
  for tc = test_suite.test_cases_list
    for t = tc'
      writeCase(fp, t, eol);
    end
  end
  fprintf(fp, ['  </testsuite>', eol]);
end
