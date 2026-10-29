%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The JSON facade is testable headless; the window itself needs a display.
%=============================================================================
txt = nmm_gui_rpc('list', '{}');
assert_istrue(ischar(txt));
r = jsondecode(txt);
assert_istrue(r.ok);
assert_istrue(isfield(r, 'installed'));
%=============================================================================
txt = nmm_gui_rpc('unknown', '{}');
r = jsondecode(txt);
assert_isfalse(r.ok);
assert_istrue(isfield(r, 'error'));
%=============================================================================
txt = nmm_gui_rpc('registry', '');
r = jsondecode(txt);
assert_istrue(isfield(r, 'ok'));
%=============================================================================
% Failures carry their identifier: the window only says "no registry
% configured" for that case, "registry unavailable" otherwise.
previousRegistry = getenv('NELSON_NMM_REGISTRY');
missing = [tempname(), '_missing_registry.json'];
setenv('NELSON_NMM_REGISTRY', missing);
notConfigured = jsondecode(nmm_gui_rpc('search', '{"query":""}'));
filewrite(missing, '{"packages":[]}');
unreadable = jsondecode(nmm_gui_rpc('search', '{"query":""}'));
delete(missing);
setenv('NELSON_NMM_REGISTRY', previousRegistry);
assert_isfalse(notConfigured.ok);
assert_isequal(notConfigured.error_id, 'Nelson:nmm:RegistryNotConfigured');
assert_isfalse(unreadable.ok);
assert_isequal(unreadable.error_id, 'Nelson:nmm:RegistryChecksumMissing');
%=============================================================================
% snapshot = list + search + registry status in one call, each part with its
% own ok; the registry reuse switch it sets is restored afterwards.
previousReuse = getenv('NELSON_NMM_REGISTRY_REUSE');
r = jsondecode(nmm_gui_rpc('snapshot', '{}'));
assert_istrue(r.ok);
assert_istrue(r.list.ok);
assert_istrue(isfield(r.list, 'installed'));
assert_istrue(isfield(r.search, 'ok'));
assert_istrue(isfield(r.registry, 'ok'));
assert_isequal(getenv('NELSON_NMM_REGISTRY_REUSE'), previousReuse);
% a user refresh (fresh) bypasses the on-disk registry cache for that call only
previousTtl = getenv('NELSON_NMM_REGISTRY_CACHE_TTL');
r = jsondecode(nmm_gui_rpc('snapshot', '{"fresh":true}'));
assert_istrue(r.ok);
assert_istrue(r.list.ok);
assert_isequal(getenv('NELSON_NMM_REGISTRY_CACHE_TTL'), previousTtl);
%=============================================================================
% Registry entries with heterogeneous fields: a field an entry lacks must not
% reach the window as an empty array (JS treats [] as a present value).
registry = [tempname(), '_registry.json'];
fid = fopen(registry, 'wt');
fprintf(fid, '%s', '{"packages":[{"name":"pa","version":"1.0.0","platforms":["all"],"source":"https://h/pa-1.0.0.nmz"},{"name":"pb","version":"1.0.0","platforms":["all"],"homepage":"https://h/pb"}]}');
fclose(fid);
filewrite([registry, '.sha256'], sha256(registry, '-file'));
previousRegistry = getenv('NELSON_NMM_REGISTRY');
setenv('NELSON_NMM_REGISTRY', registry);
r = jsondecode(nmm_gui_rpc('search', '{"query":""}'));
setenv('NELSON_NMM_REGISTRY', previousRegistry);
delete(registry);
delete([registry, '.sha256']);
assert_istrue(r.ok);
names = cellfun(@(p) p.name, r.packages, 'UniformOutput', false);
pa = r.packages{strcmp(names, 'pa')};
pb = r.packages{strcmp(names, 'pb')};
assert_isequal(pa.source, 'https://h/pa-1.0.0.nmz');
assert_isfalse(isfield(pa, 'homepage'));
assert_isequal(pb.homepage, 'https://h/pb');
assert_isfalse(isfield(pb, 'source'));
%=============================================================================
% An installed version later yanked from the registry is flagged by 'list'
% (the registry is read once for all installed packages).
testRoot = [tempname(), '_nmm_gui/'];
moduleName = 'nmm_gui_yank_probe';
moduleRoot = [testRoot, 'src/'];
mkdir(testRoot);
mkdir(moduleRoot);
mkdir([moduleRoot, 'etc']);
mkdir([moduleRoot, 'tests']);
mkdir([moduleRoot, 'help/en_US/xml']);
filewrite([moduleRoot, 'help/en_US/xml/chapter.xml'], '<chapter></chapter>');
filewrite([moduleRoot, 'tests/test_placeholder.m'], '');
filewrite([moduleRoot, 'module.json'], ['{"module":"', moduleName, '","title":"probe","summary":"probe","version":"1.0.0","platforms":["all"],"nelson":">=1.0.0","builtin":false,"license":"MIT","dependencies":{}}']);
filewrite([moduleRoot, 'builder.m'], ['nmm_build_loader(''', moduleName, ''', fileparts(nfilename(''fullpath'')));']);
filewrite([moduleRoot, 'etc/startup.m'], '');
filewrite([moduleRoot, 'etc/finish.m'], '');
registry = [testRoot, 'registry.json'];
filewrite(registry, ['{"packages":[{"name":"', moduleName, '","version":"1.0.0","platforms":["all"],"source":"https://h/p.nmz","yanked":true,"yank_reason":"superseded"}]}']);
filewrite([registry, '.sha256'], sha256(registry, '-file'));
previousModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousRegistry = getenv('NELSON_NMM_REGISTRY');
setenv('NELSON_EXTERNAL_MODULES_PATH', [testRoot, 'modules/']);
setenv('NELSON_NMM_REGISTRY', registry);
try
  nmm('install', moduleRoot);
  r = jsondecode(nmm_gui_rpc('list', '{}'));
  nmm('uninstall', moduleName);
catch e
  r = struct('ok', false, 'error', e.message);
end
setenv('NELSON_EXTERNAL_MODULES_PATH', previousModulesPath);
setenv('NELSON_NMM_REGISTRY', previousRegistry);
rmdir(testRoot, 's');
assert_istrue(r.ok);
installed = r.installed;
if isstruct(installed)
  installed = num2cell(installed);
end
names = cellfun(@(p) p.name, installed, 'UniformOutput', false);
probe = installed{strcmp(names, moduleName)};
assert_istrue(probe.yanked);
assert_isequal(probe.yank_reason, 'superseded');
% size on disk and install date of the installed package
assert_istrue(probe.bytes > 0);
assert_istrue(~isempty(regexp(probe.installed_at, '^\d{4}-\d{2}-\d{2} \d{2}:\d{2}$', 'once')));
%=============================================================================
% UI catalogs (locale/nmm-ui-<locale>.json) are embedded in the binary:
% every translation carries exactly the keys of the English reference.
locales = __nmm_gui_i18n__();
assert_istrue(any(strcmp(locales, 'en_US')));
assert_istrue(any(strcmp(locales, 'fr_FR')));
assert_isequal(__nmm_gui_i18n__('xx_XX'), '');
english = __nmm_gui_i18n__('en_US');
assert_istrue(isfield(jsondecode(english), 'title'));
assert_istrue(contains(__nmm_gui_i18n__('fr_FR'), 'Mises à jour'));
referenceKeys = regexp(english, '"([^"]+)":', 'tokens');
referenceKeys = sort([referenceKeys{:}]);
for k = 1:numel(locales)
  keys = regexp(__nmm_gui_i18n__(locales{k}), '"([^"]+)":', 'tokens');
  assert_isequal(sort([keys{:}]), referenceKeys);
end
% (the key usage by the page is checked by the frontend unit tests)
%=============================================================================
% Install preview: dry-run plan and registry entry of the version, computed
% without network for a local registry.
registry = [tempname(), '_registry.json'];
filewrite(registry, '{"packages":[{"name":"pa","version":"1.0.0","platforms":["all"],"license":"MIT","size":2048,"source":"https://h.invalid/pa-1.0.0.nmz","checksum":"abc"}]}');
filewrite([registry, '.sha256'], sha256(registry, '-file'));
previousRegistry = getenv('NELSON_NMM_REGISTRY');
setenv('NELSON_NMM_REGISTRY', registry);
r = jsondecode(nmm_gui_rpc('preview', '{"name":"pa"}'));
setenv('NELSON_NMM_REGISTRY', previousRegistry);
delete(registry);
delete([registry, '.sha256']);
assert_istrue(r.ok);
assert_isequal(r.plan.install_plan.package, 'pa');
assert_isequal(r.package.license, 'MIT');
assert_isequal(r.package.size, 2048);
% a readme is only fetched from a GitHub or GitLab repository
r = jsondecode(nmm_gui_rpc('doc', '{"kind":"readme","path":"","repository":"https://example.invalid/x"}'));
assert_istrue(r.ok);
assert_isequal(r.text, '');
%=============================================================================
% Window preferences kept in prefdir/nmm_gui.json (restored afterwards).
prefsFile = fullfile(prefdir(), 'nmm_gui.json');
hadPrefs = isfile(prefsFile);
if hadPrefs
  previousPrefs = fileread(prefsFile);
end
merged = jsondecode(__nmm_gui_prefs__('{"filter":"installed","width":1100}'));
assert_isequal(merged.filter, 'installed');
assert_isequal(merged.width, 1100);
assert_isequal(jsondecode(__nmm_gui_prefs__()).filter, 'installed');
assert_checkerror('__nmm_gui_prefs__(''[1,2]'')', _('Preferences must be a JSON object.'), 'Nelson:nmm_gui:invalidPrefs');
if hadPrefs
  filewrite(prefsFile, previousPrefs);
else
  delete(prefsFile);
end
%=============================================================================
% Update count shown on the desktop Package Manager badges (set by the window
% after each load; 0 until then, never negative).
previousUpdates = __nmm_gui_updates__();
assert_isequal(__nmm_gui_updates__(3), 3);
assert_isequal(__nmm_gui_updates__(), 3);
assert_isequal(__nmm_gui_updates__(-2), 0);
__nmm_gui_updates__(previousUpdates);
%=============================================================================
% Progress listener: forwards to the window when open, silent otherwise.
assert_isequal(nargin('__nmm_gui_notify__'), 2);
nmm_progress_listener('install', 'demo', '1.0.0', 'build', 3, 6);
__nmm_gui_notify__('progress', '{"name":"demo","step":"build"}');
__nmm_gui_notify__('failed', 'message with ''quotes'' and "double quotes"');
%=============================================================================
% Deep link nmm_gui(action, name[, version]): arguments are validated before
% any window is opened, so these stay headless.
assert_checkerror('nmm_gui(''bogus'', ''ngen'')', _('Valid actions are ''show'' and ''install''.'), 'Nelson:nmm_gui:invalidAction');
assert_checkerror('nmm_gui(''install'')', _('A package name is expected.'), 'Nelson:nmm_gui:nameExpected');
assert_checkerror('nmm_gui(''install'', ''bad name'')', _('Invalid package name.'), 'Nelson:nmm_gui:invalidName');
assert_checkerror('nmm_gui(''install'', ''ngen'', ''!!'')', _('Invalid package version.'), 'Nelson:nmm_gui:invalidVersion');
%=============================================================================
