%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function status = report_writeHtml(results, outputFile)
  if isempty(outputFile)
    error('Nelson:tests_manager:outputFilenameExpected', 'Output filename expected.');
  end
  cases = report_htmlCases(results);
  title = report_htmlTitle(results);
  lines = {
    '<!doctype html>'
    '<html lang="en">'
    '<head>'
    '<meta charset="utf-8">'
    '<meta name="viewport" content="width=device-width, initial-scale=1">'
    ['<title>', report_htmlEscape(title), '</title>']
  };
  lines = lines(:);
  lines = [lines; report_htmlStyles()];
  lines = [lines; {'</head>'; '<body>'; '<main>'}];
  lines = [lines; report_writeHtmlHeader(results, title)];
  lines = [lines; report_htmlRunnerConfig(results)];
  lines = [lines; report_htmlSummary(results, cases)];
  lines = [lines; report_htmlRunInfo(results)];
  lines = [lines; report_htmlOverview(cases)];
  lines = [lines; report_writeHtmlToolbar()];
  lines = [lines; report_writeHtmlTable(cases)];
  lines = [lines; report_htmlScript(results)];
  lines = [lines; {'</main>'; '</body>'; '</html>'}];
  filewrite(outputFile, lines);
  report_htmlWriteSidecarJson(results, outputFile);
  status = isfile(outputFile);
end
%=============================================================================
function lines = report_writeHtmlHeader(results, title)
  statusText = 'failed';
  if isfield(results, 'status') && results.status
    statusText = 'passed';
  end
  lines = {
    '<header>'
    '<div>'
    ['<h1>', report_htmlEscape(title), '</h1>']
    ['<div class="meta">Run status: ', report_htmlEscape(statusText), '</div>']
    '</div>'
    '</header>'
  };
  lines = lines(:);
end
%=============================================================================
function title = report_htmlTitle(results)
  tests = report_htmlSummaryCount(results, 'tests');
  bench = report_htmlSummaryCount(results, 'bench');
  if bench > 0 && tests == 0
    title = 'Nelson Benchmark Report';
  elseif bench > 0
    title = 'Nelson Test and Benchmark Report';
  else
    title = 'Nelson Test Report';
  end
end
%=============================================================================
function value = report_htmlSummaryCount(results, name)
  value = 0;
  if isfield(results, 'summary')
    value = report_summaryValue(results.summary, name);
  end
end
%=============================================================================
function lines = report_writeHtmlToolbar()
  lines = {
    '<section class="toolbar" aria-label="Filters">'
    '<input id="q" type="search" placeholder="Filter by name, module, kind, or tag" aria-label="Filter tests">'
    '<select id="outcome" aria-label="Filter by outcome">'
    '<option value="">All outcomes</option>'
    '<option value="passed">Passed</option>'
    '<option value="failed">Failed</option>'
    '<option value="skipped">Skipped</option>'
    '<option value="timeout">Timeout</option>'
    '<option value="error">Error</option>'
    '<option value="bench">Bench</option>'
    '</select>'
    '<button class="quick-filter" type="button" data-quick="failed">Failed</button>'
    '<button class="quick-filter" type="button" data-quick="skipped">Skipped</button>'
    '<button class="quick-filter" type="button" data-quick="bench">Bench</button>'
    '<button class="quick-filter" type="button" data-quick="slow">Slow &gt; 1s</button>'
    '</section>'
  };
  lines = lines(:);
end
%=============================================================================
function lines = report_writeHtmlTable(cases)
  lines = {
    '<table>'
    ['<thead><tr><th data-sort="index">#</th><th data-sort="outcome">Outcome</th>', ...
    '<th data-sort="module">Module</th><th data-sort="name">Name</th>', ...
    '<th data-sort="kind">Kind</th><th data-sort="duration">Seconds</th><th>Tags</th></tr></thead>']
    '<tbody>'
  };
  lines = [lines; report_htmlCaseRows(cases)];
  lines = [lines; {'</tbody>'; '</table>'}];
  lines = lines(:);
end
%=============================================================================
