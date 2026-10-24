%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~ismodule('compiler'), 'compiler module is not installed.');
%=============================================================================
% compiler is a permanent module: it is loaded at startup and cannot be
% removed. The ncc driver macro exposes its builtins and link plan, and must
% not leak path, working directory, or environment changes to the caller.
%=============================================================================
beforePath = path();
beforeDirectory = pwd();
beforeEnvironment = getenv('NELSONC_INVOKING_DIRECTORY');
asserts.istrue(ismodule('compiler'));
asserts.isfalse(ismodule('ncc'));
asserts.istrue(endsWith(which('ncc'), 'ncc.m'));
asserts.istrue(isempty(which('nelsonc')));
asserts.isfalse(isfile(fullfile(modulepath('modules_manager'), 'functions', 'nelsonc.m')));
asserts.isfalse(isempty(which('__nelsonc_plan__')));
asserts.istrue(endsWith(modulepath('compiler'), 'compiler'));
asserts.istrue(contains(modulepath('compiler', 'builtin'), 'libnlsCompiler_builtin'));
usage = ncc('--help');
asserts.istrue(contains(usage, 'usage: nelsonc'));
asserts.istrue(contains(usage, 'Nelson: output = ncc('));
asserts.isequal(ncc(), usage);
asserts.isequal(ncc("--help"), usage);
commandHelp = evalc('ncc --help');
asserts.istrue(contains(commandHelp, 'Nelson: output = ncc('));
asserts.isequal(path(), beforePath);
asserts.isequal(pwd(), beforeDirectory);
asserts.isequal(getenv('NELSONC_INVOKING_DIRECTORY'), beforeEnvironment);
assert_arguments_rejected({17});
assert_arguments_rejected({['a'; 'b']});
assert_arguments_rejected({["a", "b"]});
assert_arguments_rejected({'entry.m', '-o'});
assert_arguments_rejected({'entry.m', '-o', ''});
assert_arguments_rejected({'--unknown'});
assert_arguments_rejected({'entry.m', '--gui', '--cli'});
assert_arguments_rejected({'entry.m', '--runtime', 'unknown'});
assert_arguments_rejected({'entry.m', '--runtime', '--help'});
assert_arguments_rejected({'entry.m'});
assert_arguments_rejected({'--explain-link'});
assert_arguments_rejected({'first.m', 'second.m'});
entry = fullfile(nelsonroot(), 'modules', 'main', 'tests', 'fixtures', 'nelsonc', 'app_entry.m');
plan = ncc(entry, '--runtime', 'installed', '--explain-link');
asserts.istrue(isstruct(plan));
asserts.istrue(plan.complete);
asserts.isequal(plan.runtime.mode, 'installed');
asserts.isfalse(plan.runtime.gui);
asserts.isfalse(any(strcmp(plan.runtime.modules, 'compiler')));
asserts.isequal(path(), beforePath);
asserts.isequal(pwd(), beforeDirectory);
asserts.isequal(getenv('NELSONC_INVOKING_DIRECTORY'), beforeEnvironment);
%=============================================================================
function assert_arguments_rejected(arguments)
  identifier = '';
  try
    ncc(arguments{:});
  catch ex
    identifier = ex.identifier;
  end
  asserts.istrue(strcmp(identifier, 'Nelson:nelsonc:arguments') || startsWith(identifier, 'Nelson:nelsonc:arguments:'));
end
%=============================================================================
