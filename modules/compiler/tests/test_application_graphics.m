%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
previousUserPath = userpath();
userpath('clear');
restoreUserPath = onCleanup(@() userpath(previousUserPath));
entry = fullfile(nelsonroot(), 'modules', 'main', 'tests', 'fixtures', 'nelsonc_graphics', 'graphics_entry.m');
before = findall(groot(), 'Type', 'figure');
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete);
asserts.isfalse(plan.dynamicFallback);
asserts.istrue(plan.requiresGraphics);
asserts.istrue(any(strcmp(plan.graphicsModules, 'graphics')));
asserts.isequal(findall(groot(), 'Type', 'figure'), before);
asserts.istrue(__nelsonc_select_mode__(plan, 'auto'));
asserts.istrue(__nelsonc_select_mode__(plan, 'gui'));
asserts.istrue(__nelsonc_select_mode__(plan, true));
assert_rejected(@() __nelsonc_select_mode__(plan, 'cli'));
assert_rejected(@() __nelsonc_select_mode__(plan, false));
assert_rejected(@() __nelsonc_plan__('runtime', jsonencode(plan), false));
runtime = jsondecode(__nelsonc_plan__('runtime', jsonencode(plan), true));
asserts.istrue(any(strcmp(runtime.modules, 'graphics')));
asserts.isfalse(any(strcmp(runtime.modules, 'qml_engine')));
% A compiled GUI application ships the gui frontend of the native WebView
% desktop and no windowing-toolkit plugins.
guiRuntime = struct('gui', true, 'modules', {{'graphics', 'gui'}});
folders = __nelsonc_graphics_assets__(guiRuntime);
asserts.istrue(any(strcmp(folders, 'gui')));
asserts.isfalse(any(strcmp(folders, 'plugins/platforms')));
asserts.isfalse(any(strcmp(folders, 'qml')));
libraryDirectory = modulepath('nelson', 'builtin');
root = __nelsonc_graphics_root__(guiRuntime, libraryDirectory);
webgui = __nelsonc_canonical_path__(__nelsonc_graphics_source__(root, 'gui'));
asserts.istrue(isfolder(webgui));
if ismac()
  bundle = __nelsonc_bundle_contents__(libraryDirectory);
  if ~isempty(bundle)
    % Installed .app bundle: the graphics root is the bundle Contents folder,
    % with gui under Resources.
    asserts.isequal(root, bundle);
  end
  asserts.istrue(isfile(fullfile(nelsonroot(), 'resources', 'help-icon.svg')));
  asserts.istrue(isfile(fullfile(nelsonroot(), 'resources', 'zoom-in.svg')));
  asserts.istrue(isfile(fullfile(nelsonroot(), 'modules', 'graphics', 'resources', ...
    'icon-mathematical-plot.svg')));
end
withoutGui = struct('gui', false, 'modules', {{'graphics', 'gui'}});
asserts.istrue(isempty(__nelsonc_graphics_assets__(withoutGui)));
for name = {'api-ms-win-core-file-l1-1-0.dll', 'EXT-MS-ONECORE-APPMODEL-STATEREPOSITORY-CACHE-L1-1-0.DLL'}
  asserts.istrue(__nelsonc_is_system_library__(name{1}, ''));
end
for name = {'api-plugin.dll', 'ext-module.dll', 'kernel32.dll'}
  asserts.isfalse(__nelsonc_is_system_library__(name{1}, ''));
end
if ispc()
  systemFile = fullfile(getenv('SystemRoot'), 'System32', 'kernel32.dll');
  asserts.istrue(__nelsonc_is_system_library__('kernel32.dll', systemFile));
  systemFile = fullfile(getenv('SystemRoot'), 'System32', 'msvcrt.dll');
  asserts.istrue(__nelsonc_is_system_library__('msvcrt.dll', systemFile));
  asserts.isfalse(__nelsonc_is_system_library__('libnlsInterpreter.dll', ...
    fullfile(modulepath('nelson', 'builtin'), 'libnlsInterpreter.dll')));
  runtimeFile = fullfile(getenv('SystemRoot'), 'System32', 'vcruntime140.dll');
  if isfile(runtimeFile)
    % VC runtime DLLs (vcruntime*, msvcp*, vcomp*, ...) are system libraries:
    % they are provided by the redistributable, not staged as user dependencies.
    asserts.istrue(__nelsonc_is_system_library__('vcruntime140.dll', runtimeFile));
  end
end

local = fullfile(directory, 'local_plot.m');
write_lines(local, {'function value = local_plot()', 'value = plot();', 'end', ...
  'function value = plot()', 'value = ''figure'';', 'end'});
plain = jsondecode(__nelsonc_plan__('link', local));
asserts.istrue(plain.complete);
asserts.isfalse(plain.requiresGraphics);
asserts.isfalse(__nelsonc_select_mode__(plain, 'auto'));
asserts.isfalse(__nelsonc_select_mode__(plain, 'cli'));
asserts.istrue(__nelsonc_select_mode__(plain, 'gui'));
assert_rejected(@() __nelsonc_select_mode__(plain, 'unknown'));
assert_rejected(@() __nelsonc_select_mode__(plain, [false, true]));

helper = fullfile(directory, 'draw_helper.m');
write_lines(helper, {'function draw_helper()', 'figure();', 'end'});
write_lines(local, {'function local_plot()', 'draw_helper();', 'end'});
transitive = jsondecode(__nelsonc_plan__('link', local));
asserts.istrue(transitive.complete);
asserts.istrue(transitive.requiresGraphics);
write_lines(local, {'function local_plot(name)', 'feval(name);', 'end'});
dynamic = jsondecode(__nelsonc_plan__('link', local));
asserts.istrue(dynamic.dynamicFallback);
asserts.istrue(dynamic.requiresGraphics);
clear restoreUserPath cleanup;

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  fprintf(fid, '%s\n', lines{:});
  fclose(fid);
end

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
