%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function testFile = createDiscoveryModule(root, name, selfLoad)
  mkdir(root);
  mkdir(fullfile(root, 'etc'));
  mkdir(fullfile(root, 'functions'));
  mkdir(fullfile(root, 'tests'));
  mkdir(fullfile(root, 'help', 'en_US', 'xml'));
  info = struct('module', name, 'title', 'Module discovery fixture', ...
    'summary', 'Local test of module identity', 'version', '1.0.0', ...
    'platforms', {{'all'}}, 'nelson', '>=1.0.0', 'builtin', false, ...
    'license', 'LGPL-3.0-or-later', 'dependencies', struct());
  filewrite(fullfile(root, 'module.json'), jsonencode(info));
  filewrite(fullfile(root, 'builder.m'), ...
    {['nmm_build_loader(''', name, ''',fileparts(nfilename(''fullpath'')));']});
  filewrite(fullfile(root, 'etc', 'startup.m'), ...
    {['addpath([modulepath(''', name, '''),''/functions'']);']});
  filewrite(fullfile(root, 'etc', 'finish.m'), ...
    {['rmpath([modulepath(''', name, '''),''/functions'']);']});
  filewrite(fullfile(root, 'functions', [name, '_value.m']), ...
    {['function value = ', name, '_value()']; 'value = 42;'; 'end'});
  filewrite(fullfile(root, 'help', 'en_US', 'xml', 'chapter.xml'), '<chapter></chapter>');
  code = {};
  if selfLoad
    code = {['if ~ismodule(''', name, ''')']; ...
      '  run([fileparts(fileparts(nfilename(''fullpath''))),''/loader.m'']);'; 'end'};
  end
  code = [code; {['asserts.istrue(ismodule(''', name, '''));']; ...
    ['asserts.isequal(', name, '_value(),42);']; ...
    'asserts.isfalse(ismodule(''.nmm-tmp''));'}];
  testFile = fullfile(root, 'tests', 'test_identity.m');
  filewrite(testFile, code);
  run(fullfile(root, 'builder.m'));
end
