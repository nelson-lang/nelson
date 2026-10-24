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
asserts.istrue(any(strcmp(runtime.modules, 'gui')));
asserts.isfalse(any(strcmp(runtime.modules, 'qml_engine')));
runtime.gui = true;
folders = __nelsonc_graphics_assets__(runtime);
asserts.istrue(any(strcmp(folders, 'plugins/platforms')));
asserts.istrue(any(strcmp(folders, 'plugins/iconengines')));
asserts.isfalse(any(strcmp(folders, 'qml')));
asserts.isfalse(any(strcmp(folders, 'plugins/designer')));
if ismac()
  libraryDirectory = modulepath('nelson', 'builtin');
  guiInfo = jsondecode(__library_dependencies__(fullfile(libraryDirectory, 'libnlsGui.dylib')));
  qtCore = guiInfo.needed{find(contains(guiInfo.needed, '/QtCore.framework/'), 1)};
  runtime.libraries = struct('filename', 'QtCore', 'source', qtCore);
  root = __nelsonc_graphics_root__(runtime, libraryDirectory);
  bundle = __nelsonc_bundle_contents__(libraryDirectory);
  % The platform plugin resolves in both the development and installed-bundle
  % layouts; the source resolver maps Contents/PlugIns for an installed bundle.
  platforms = __nelsonc_canonical_path__(__nelsonc_graphics_source__(root, 'plugins/platforms'));
  asserts.istrue(isfolder(platforms));
  asserts.istrue(isfile(fullfile(platforms, 'libqcocoa.dylib')));
  if isempty(bundle)
    % Development/Homebrew layout ships the full plugin set under a single root.
    engine = __nelsonc_canonical_path__(__nelsonc_graphics_source__(root, 'plugins/iconengines'));
    asserts.istrue(isfile(fullfile(engine, 'libqsvgicon.dylib')));
  else
    % Installed .app bundle: the graphics root is the bundle Contents folder,
    % with Qt plugins under PlugIns and qml under Resources.
    asserts.isequal(root, bundle);
  end
  asserts.istrue(isfile(fullfile(nelsonroot(), 'resources', 'help-icon.svg')));
  asserts.istrue(isfile(fullfile(nelsonroot(), 'resources', 'zoom-in.svg')));
  asserts.istrue(isfile(fullfile(nelsonroot(), 'modules', 'graphics', 'resources', ...
    'icon-mathematical-plot.svg')));
end
runtime.modules{end + 1} = 'qml_engine';
asserts.istrue(any(strcmp(__nelsonc_graphics_assets__(runtime), 'qml')));
runtime.gui = false;
asserts.istrue(isempty(__nelsonc_graphics_assets__(runtime)));
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
