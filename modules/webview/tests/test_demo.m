%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('examples gallery tests require GUI mode.'));
%=============================================================================
asserts.isequal(nargin('demo'), 0);
asserts.isequal(nargout('demo'), 0);
%=============================================================================
% Repeated calls reuse the gallery and refresh newly loaded toolboxes.
demo();
[firstWindow, firstGeneration, initialCatalog] = __webview__('examplesState');
asserts.istrue(firstWindow > 0);
asserts.istrue(firstGeneration > 0);

demo();
[secondWindow, secondGeneration, refreshedCatalog] = __webview__('examplesState');
asserts.isequal(secondWindow, firstWindow);
asserts.isequal(secondGeneration, firstGeneration);
asserts.istrue(strlength(refreshedCatalog) >= strlength(initialCatalog));

moduleRoot = tempname();
moduleName = ['demo_test_', strrep(createGUID(), '-', '_')];
cleanup = onCleanup(@() cleanupDemoToolbox(moduleName, moduleRoot));
mkdir(moduleRoot);
mkdir(fullfile(moduleRoot, 'etc'));
mkdir(fullfile(moduleRoot, 'examples'));
filewrite(fullfile(moduleRoot, 'etc', 'startup.m'), '');
filewrite(fullfile(moduleRoot, 'etc', 'finish.m'), '');
filewrite(fullfile(moduleRoot, 'examples', 'toolbox_example.m'), 'disp(''toolbox example'');');
manifest = ['{"schemaVersion":1,"examples":[{"file":"toolbox_example.m",', ...
  '"title":{"en_US":"Toolbox example","fr_FR":"Exemple de toolbox"},', ...
  '"description":{"en_US":"Loaded dynamically.","fr_FR":"Chargé dynamiquement."}}]}'];
filewrite(fullfile(moduleRoot, 'examples', 'index.json'), manifest);
filewrite(fullfile(moduleRoot, 'module.json'), ...
  ['{"title":"', moduleName, '","version":"1.0.0"}']);
addmodule(moduleRoot, moduleName);

demo();
[toolboxWindow, toolboxGeneration, toolboxCatalog] = __webview__('examplesState');
asserts.isequal(toolboxWindow, firstWindow);
asserts.isequal(toolboxGeneration, firstGeneration);
asserts.istrue(contains(toolboxCatalog, moduleName));

__webview__('closeAll');
[closedWindow, closedGeneration, closedCatalog] = __webview__('examplesState');
asserts.isequal(closedWindow, 0);
asserts.isequal(closedGeneration, firstGeneration);
asserts.isequal(closedCatalog, '');
demo();
[thirdWindow, thirdGeneration, recreatedCatalog] = __webview__('examplesState');
asserts.istrue(thirdWindow > 0);
asserts.istrue(thirdGeneration > firstGeneration);
asserts.istrue(contains(recreatedCatalog, moduleName));
__webview__('closeAll');
removemodule(moduleName);
clear cleanup;
%=============================================================================
function cleanupDemoToolbox(moduleName, moduleRoot)
  __webview__('closeAll');
  if ismodule(moduleName)
    removemodule(moduleName);
  end
  if isdir(moduleRoot)
    rmdir(moduleRoot, 's');
  end
end
%=============================================================================
