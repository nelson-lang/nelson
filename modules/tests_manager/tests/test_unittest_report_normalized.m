%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_report_normalized_', createGUID()];
jsonFile = [base, '.json'];
junitFile = [base, '.xml'];
tapFile = [base, '.tap'];
htmlFile = [base, '_report.html'];
htmlJsonFile = [base, '_report.json'];
cleanupJson = onCleanup(@() evalc(removeCommand(jsonFile))); %#ok<NLS0004>
cleanupJUnit = onCleanup(@() evalc(removeCommand(junitFile))); %#ok<NLS0004>
cleanupTap = onCleanup(@() evalc(removeCommand(tapFile))); %#ok<NLS0004>
cleanupHtml = onCleanup(@() evalc(removeCommand(htmlFile))); %#ok<NLS0004>
cleanupHtmlJson = onCleanup(@() evalc(removeCommand(htmlJsonFile))); %#ok<NLS0004>

results = struct();
results.type = 'TestRunResult';
results.status = false;
results.summary = struct('passed', 1, 'failed', 1, 'skipped', 1, ...
  'filtered', 0, 'timeout', 1, 'error', 0, 'bench', 0, 'duration', 1);
results.cases(1) = struct('id', 'case_pass', 'module', 'synthetic_module', ...
  'outcome', 'passed', 'duration', 0.1, ...
  'message', '', 'stdout', ['out', char(8), '100 %'], 'stderr', '', 'exit_code', 0, 'attempts', 1);
results.cases(2) = struct('id', 'case_fail', 'module', 'synthetic_module', ...
  'outcome', 'failed', 'duration', 0.2, ...
  'message', 'failure message <script>alert(1)</script> ]]> tail', ...
  'stdout', '', 'stderr', 'err', 'exit_code', 1, 'attempts', 1);
results.cases(3) = struct('id', 'case_skip', 'module', 'synthetic_module', ...
  'outcome', 'skipped', 'duration', 0, ...
  'message', '', 'stdout', '', 'stderr', '', 'exit_code', 0, 'attempts', 1);
results.cases(4) = struct('id', 'case_timeout', 'module', 'synthetic_module', ...
  'outcome', 'timeout', 'duration', 3, ...
  'message', 'timeout message', 'stdout', '', 'stderr', '', 'exit_code', 258, 'attempts', 1);
rawCases(1) = struct('id', 'case_pass', 'classname', 'Nelson-2.0.0.0.synthetic_module', ...
  'filename', [nelsonroot(), '/modules/synthetic_module/tests/case_pass.m'], 'kind', 'test');
rawCases(2) = struct('id', 'case_fail', 'classname', 'Nelson-2.0.0.0.synthetic_module', ...
  'filename', [nelsonroot(), '/modules/synthetic_module/tests/case_fail.m'], 'kind', 'test');
rawCases(3) = struct('id', 'case_skip', 'classname', 'Nelson-2.0.0.0.synthetic_module', ...
  'filename', [nelsonroot(), '/modules/synthetic_module/tests/case_skip.m'], 'kind', 'test');
rawCases(4) = struct('id', 'case_timeout', 'classname', 'Nelson-2.0.0.0.synthetic_module', ...
  'filename', [nelsonroot(), '/modules/synthetic_module/tests/case_timeout.m'], 'kind', 'test');
results.raw.tests_list = struct('test_cases_list', rawCases);

asserts.istrue(nelson.unittest.report(results, 'Format', 'json', 'OutputFile', jsonFile));
asserts.istrue(nelson.unittest.report(results, 'Format', 'junit', 'OutputFile', junitFile));
asserts.istrue(nelson.unittest.report(results, 'Format', 'tap', 'OutputFile', tapFile));
asserts.istrue(nelson.unittest.report(results, 'Format', 'html', 'OutputFile', htmlFile));
asserts.istrue(isfile(htmlJsonFile));

concise = evalc('nelson.unittest.report(results, ''Format'', ''console'');');
verbose = evalc('nelson.unittest.report(results, ''Format'', ''console'', ''Verbose'', true);');

junit = fileread(junitFile);
tap = fileread(tapFile);
html = fileread(htmlFile);
asserts.istrue(contains(concise, 'nelson.unittest: 4 tests'));
asserts.istrue(contains(concise, '1 failed'));
asserts.isfalse(contains(concise, 'case_fail'));
asserts.istrue(contains(verbose, 'failed case_fail'));
asserts.istrue(contains(verbose, 'failure message'));
xmlDoc = xmlread(junitFile);
asserts.isequal(xmlDoc.getDocumentElement().getNodeName(), "testsuites");
asserts.istrue(contains(junit, 'failures="1"'));
asserts.istrue(contains(junit, 'errors="1"'));
asserts.istrue(contains(junit, 'classname="synthetic_module"'));
asserts.istrue(contains(junit, '<skipped/>'));
asserts.istrue(contains(junit, '<error message="test error"><![CDATA[timeout message]]></error>'));
asserts.istrue(contains(junit, '<system-out><![CDATA[out100 %]]></system-out>'));
asserts.istrue(contains(junit, '<system-err><![CDATA[err]]></system-err>'));
asserts.istrue(contains(tap, 'not ok 2 - case_fail'));
asserts.istrue(contains(tap, 'message: failure message'));
asserts.istrue(contains(tap, 'ok 3 - case_skip # SKIP'));
asserts.istrue(contains(tap, 'not ok 4 - case_timeout'));
asserts.istrue(contains(html, '<!doctype html>'));
asserts.istrue(contains(html, 'Nelson Test Report'));
asserts.istrue(contains(html, 'Slowest tests / benches'));
asserts.istrue(contains(html, 'Module summary'));
asserts.istrue(contains(html, 'Runner configuration'));
asserts.istrue(contains(html, 'Environment'));
asserts.istrue(contains(html, 'Run timestamp'));
asserts.istrue(contains(html, 'Report generated'));
asserts.istrue(contains(html, 'Nelson root'));
asserts.istrue(contains(html, 'case_fail'));
asserts.istrue(contains(html, '<td>synthetic_module</td>'));
asserts.istrue(contains(html, 'class="module-row"'));
asserts.istrue(contains(html, 'data-sort="duration"'));
asserts.istrue(contains(html, 'data-quick="failed"'));
asserts.istrue(contains(html, 'data-quick="slow"'));
asserts.istrue(contains(html, 'id="details-2"'));
asserts.isfalse(contains(html, 'class="details-row hidden" id="details-2"'));
asserts.istrue(contains(html, 'nelson.unittest.run'));
asserts.istrue(contains(html, '&lt;script&gt;alert(1)&lt;/script&gt;'));
asserts.isfalse(contains(html, '<script>alert(1)</script>'));
asserts.istrue(contains(html, 'id="nelson-report-data"'));
asserts.istrue(contains(html, 'data-outcome="failed"'));
asserts.isfalse(contains(html, 'https://'));
asserts.isfalse(contains(html, 'http://'));
%=============================================================================
