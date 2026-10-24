%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The interactive 'Interactive' front is a thin input() prompt layer over the
% same programmatic core validated here; it cannot be driven from a test so
% only the programmatic path is asserted.
%=============================================================================
testRoot = [tempdir(), 'test_nmm_init/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
%=============================================================================
% programmatic manifest generation + defaults + validate
manifestRoot = [testRoot, 'demo_mod/'];
mkdir(manifestRoot);
writtenPath = nmm('init', manifestRoot, 'title', 'Demo', 'summary', 'x', 'license', 'LGPL-3.0-or-later');
asserts.istrue(ischar(writtenPath));
asserts.istrue(isfile([manifestRoot, 'module.json']));
data = jsondecode([manifestRoot, 'module.json'], '-file');
asserts.isequal(data.module, 'demo_mod');
asserts.isequal(data.title, 'Demo');
asserts.isequal(data.summary, 'x');
asserts.isequal(data.version, '1.0.0');
asserts.isequal(data.nelson, '>=2.0.0');
asserts.isfalse(data.builtin);
asserts.istrue(any(strcmp(cellstr(data.platforms), 'all')));
asserts.istrue(isstruct(data.dependencies));
asserts.istrue(isempty(fieldnames(data.dependencies)));
asserts.isequal(data.license, 'LGPL-3.0-or-later');
%=============================================================================
% the generated manifest passes nmm('validate') once the source tree exists
filewrite([manifestRoot, 'builder.m'], '');
mkdir([manifestRoot, 'etc']);
filewrite([manifestRoot, 'etc/startup.m'], '');
filewrite([manifestRoot, 'etc/finish.m'], '');
mkdir([manifestRoot, 'help']);
mkdir([manifestRoot, 'tests']);
asserts.istrue(nmm('validate', manifestRoot));
%=============================================================================
% nmm('validate') also accepts a lone module.json file (not only a directory)
manifestFile = [manifestRoot, 'module.json'];
asserts.istrue(nmm('validate', manifestFile));
%=============================================================================
% the generated manifest is missing recommended fields: it still writes and
% validates, but the warnings linter surfaces non-blocking suggestions
fileWarnings = nmm('validate', manifestFile, '-warnings');
asserts.istrue(fileWarnings.ok);
asserts.istrue(iscell(fileWarnings.warnings));
asserts.istrue(~isempty(fileWarnings.warnings));
asserts.istrue(any(strcmp(fileWarnings.warnings, 'module.json field is missing: repository')));
%=============================================================================
% DryRun returns the struct (with a warnings field) and writes nothing
dryRoot = [testRoot, 'dry_mod/'];
mkdir(dryRoot);
report = nmm('init', dryRoot, 'title', 'Dry', 'summary', 'y', 'license', 'MIT', 'DryRun', true);
asserts.istrue(isstruct(report));
asserts.isequal(report.module, 'dry_mod');
asserts.istrue(iscell(report.warnings));
asserts.istrue(~isempty(report.warnings));
asserts.isfalse(isfile([dryRoot, 'module.json']));
%=============================================================================
% init echoes the best-practice suggestions to the user (non-fatal)
echoRoot = [testRoot, 'echo_mod/'];
mkdir(echoRoot);
echoed = evalc('nmm(''init'', echoRoot, ''title'', ''Echo'', ''summary'', ''s'', ''license'', ''MIT'');');
asserts.istrue(contains(echoed, 'module.json field is missing: repository'));
asserts.istrue(isfile([echoRoot, 'module.json']));
asserts.istrue(nmm('validate', [echoRoot, 'module.json']));
%=============================================================================
% overwrite refused without Force, allowed with Force
overwriteId = '';
try
  nmm('init', manifestRoot, 'title', 'Demo', 'summary', 'x', 'license', 'MIT');
catch overwriteError
  overwriteId = overwriteError.identifier;
end
asserts.isequal(overwriteId, 'Nelson:nmm:ModuleJsonAlreadyExists');
nmm('init', manifestRoot, 'title', 'Demo2', 'summary', 'x', 'license', 'MIT', 'Force', true);
data2 = jsondecode([manifestRoot, 'module.json'], '-file');
asserts.isequal(data2.title, 'Demo2');
%=============================================================================
% invalid fields are rejected by nmm's own schema validator
badRoot = [testRoot, 'bad_mod/'];
mkdir(badRoot);
asserts.checkerror(['nmm(''init'', ''', badRoot, ''', ''module'', ''1bad'', ''title'', ''t'', ''summary'', ''s'', ''license'', ''MIT'')'], ...
  _('Invalid module name.'), 'Nelson:nmm:InvalidModuleName');
asserts.checkerror(['nmm(''init'', ''', badRoot, ''', ''title'', ''t'', ''summary'', ''s'', ''license'', ''MIT'', ''platforms'', {''nope_platform''})'], ...
  'Platform not supported: nope_platform', 'Nelson:nmm:PlatformNotSupported');
badVersionId = '';
try
  nmm('init', badRoot, 'title', 't', 'summary', 's', 'license', 'MIT', 'version', 'notsemver');
catch badVersionError
  badVersionId = badVersionError.identifier;
end
asserts.isequal(badVersionId, 'Nelson:semver:cannotCompareVersions');
asserts.isfalse(isfile([badRoot, 'module.json']));
%=============================================================================
% Skeleton produces a loadable, validate-clean, packageable source module
skelRoot = [testRoot, 'skel_mod/'];
mkdir(skelRoot);
nmm('init', skelRoot, 'module', 'skel_mod', 'title', 'Skel', 'summary', 's', 'license', 'MIT', 'Skeleton', true);
asserts.istrue(isfile([skelRoot, 'module.json']));
asserts.istrue(isfile([skelRoot, 'loader.m']));
asserts.istrue(isfile([skelRoot, 'builder.m']));
asserts.istrue(isfile([skelRoot, 'etc/startup.m']));
asserts.istrue(isfile([skelRoot, 'etc/finish.m']));
asserts.istrue(nmm('validate', skelRoot));
packDest = [testRoot, 'pack/'];
mkdir(packDest);
packageFile = nmm('pack', skelRoot, packDest, '-no-tests');
asserts.istrue(isfile(packageFile));
%=============================================================================
% keywords, dependencies and platforms shapes are preserved
optRoot = [testRoot, 'opt_mod/'];
mkdir(optRoot);
deps = struct('other_module', '>=1.0.0');
nmm('init', optRoot, 'module', 'opt_mod', 'title', 'Opt', 'summary', 's', 'license', 'MIT', ...
  'platforms', {'all', 'win64'}, 'keywords', {'a', 'b'}, 'dependencies', deps);
optData = jsondecode([optRoot, 'module.json'], '-file');
asserts.isequal(sort(optData.keywords(:)'), {'a', 'b'});
asserts.isequal(optData.dependencies.other_module, '>=1.0.0');
asserts.istrue(any(strcmp(optData.platforms, 'win64')));
%=============================================================================
% issues and documentation are optional http(s) URL metadata fields:
% init writes them, and the resulting manifest passes -strict validation
issRoot = [testRoot, 'iss_mod/'];
mkdir(issRoot);
nmm('init', issRoot, 'module', 'iss_mod', 'title', 'Iss', 'summary', 's', 'license', 'MIT', ...
  'repository', 'https://example.com/repo', 'homepage', 'https://example.com', 'keywords', {'k'}, ...
  'issues', 'https://example.com/repo/issues', 'documentation', 'https://example.com/docs', 'Force', true);
issFile = [issRoot, 'module.json'];
issData = jsondecode(issFile, '-file');
asserts.isequal(issData.issues, 'https://example.com/repo/issues');
asserts.isequal(issData.documentation, 'https://example.com/docs');
asserts.istrue(nmm('validate', issFile, '-strict'));
%=============================================================================
% strict validation still passes when issues/documentation are absent: they
% are truly optional and their absence must never be an error
absData = rmfield(issData, {'issues', 'documentation'});
absFile = [issRoot, 'module_absent.json'];
filewrite(absFile, jsonprettyprint(jsonencode(absData)));
asserts.istrue(nmm('validate', absFile, '-strict'));
%=============================================================================
% strict validation rejects a non-http issues value
badIssData = issData;
badIssData.issues = 'git@example.com:repo.git';
badIssFile = [issRoot, 'module_bad_issues.json'];
filewrite(badIssFile, jsonprettyprint(jsonencode(badIssData)));
asserts.checkerror(['nmm(''validate'', ''', badIssFile, ''', ''-strict'')'], ...
  'module.json field must be an HTTP URL: issues', 'Nelson:nmm:ModuleJsonUrlInvalid');
%=============================================================================
% strict validation rejects a non-http documentation value
badDocData = issData;
badDocData.documentation = 'ftp://example.com/docs';
badDocFile = [issRoot, 'module_bad_doc.json'];
filewrite(badDocFile, jsonprettyprint(jsonencode(badDocData)));
asserts.checkerror(['nmm(''validate'', ''', badDocFile, ''', ''-strict'')'], ...
  'module.json field must be an HTTP URL: documentation', 'Nelson:nmm:ModuleJsonUrlInvalid');
%=============================================================================
rmdir(testRoot, 's');
%=============================================================================
