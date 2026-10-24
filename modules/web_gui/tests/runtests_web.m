%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Web-mode test harness. test_run cannot drive nelson-webview, so these tests
% (tagged INTERACTIVE so test_run skips them) are executed by this runner:
%
%   nelson-webview --file modules/web_gui/tests/runtests_web.m
%
% Results land in <tempdir>/nelson_web_tests_report.txt, one line per test
% (PASS/FAIL) and a final "DONE <npass> <nfail>" marker the caller can wait
% for before stopping the process.
%=============================================================================
testsDir = [nelsonroot(), '/modules/web_gui/tests'];
reportFile = [tempdir(), 'nelson_web_tests_report.txt'];
if ~ismodule('assert_functions')
  run([nelsonroot(), '/modules/assert_functions/loader.m']);
end
if ~ismodule('graphics_io')
  run([nelsonroot(), '/modules/graphics_io/loader.m']);
end
if exist('saveas') == 0
  run([modulepath('graphics_io'), '/etc/startup.m']);
end
if isfile(reportFile)
  rmfile(reportFile);
end
%=============================================================================
listing = dir([testsDir, '/test_web_*.m']);
npass = 0;
nfail = 0;
for k = 1:numel(listing)
  name = listing(k).name;
  ok = true;
  msg = '';
  try
    run([testsDir, '/', name]);
  catch e
    ok = false;
    msg = e.message;
  end
  close('all');
  fid = fopen(reportFile, 'at');
  if ok
    npass = npass + 1;
    fprintf(fid, 'PASS %s\n', name);
  else
    nfail = nfail + 1;
    fprintf(fid, 'FAIL %s: %s\n', name, msg);
  end
  fclose(fid);
end
%=============================================================================
fid = fopen(reportFile, 'at');
fprintf(fid, 'DONE %d %d\n', npass, nfail);
fclose(fid);
exit(double(nfail > 0));
%=============================================================================
