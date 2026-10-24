%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
if ispc()
  % Windows: batch launcher, and nelson-webview.exe is the web executable itself.
  nelson_launcher = [binpath, '/nelson.bat'];
  nelson_web = [binpath, '/nelson-webview.exe'];
  nelson_web_exec = nelson_web;
else
  nelson_launcher = [binpath, '/nelson'];
  nelson_web = [binpath, '/nelson-webview'];
  nelson_web_exec = [binpath, '/nelson-webview-exec'];
end
web_timeout = 120;
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --url localhost');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:WebModeRequired'));
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --port 8080');
asserts.notEqual(status, 0);
asserts.istrue(contains(output, 'Nelson:ProgramOptions:WebModeRequired'));
%=============================================================================
if isfile(nelson_web)
  [status, output] = run_nelson_cli(nelson_launcher, ' --web --quiet --url localhost -e "try, assert(strcmp(getwebmode(), ''server'')); [u, p] = getweburl(); assert(~isempty(u)); assert(p > 0); exit(0); catch, exit(1); end"', web_timeout);
  asserts.isequal(status, 0, output);
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --no-user-startup --no-ipc --url localhost --port 18080 -e "try, assert(strcmp(getwebmode(), ''server'')); [u, p] = getweburl(); assert(contains(u, '':18080'')); assert(p == 18080); exit(0); catch, exit(1); end"', web_timeout);
  asserts.isequal(status, 0, output);
  if isfile(nelson_web_exec)
    [status, output] = run_nelson_cli(nelson_web_exec, ' --web --quiet --url 127.0.0.1 -e "try, assert(strcmp(getwebmode(), ''server'')); exit(0); catch, exit(1); end"', web_timeout);
    asserts.isequal(status, 0, output);
  end
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --port 0 -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:InvalidWebPort'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --port 65536 -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:InvalidWebPort'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --url https://localhost -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'HTTPS is not supported'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --url http://localhost/path -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'must not contain credentials, a path, query, or fragment'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --url localhost:8080 -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'must not contain a port'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --url -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:MissingValue'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --url localhost --url 127.0.0.1 -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:DuplicateOption'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --port 18080 --port 18081 -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:DuplicateOption'));
  [status, output] = run_nelson_cli(nelson_web, ' --web --quiet --port -e "quit"', web_timeout);
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:MissingValue'));
end
%=============================================================================
