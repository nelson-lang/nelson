%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isfile([modulepath('compiler'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
root = modulepath('compiler');
names = public_functions(fullfile(root, 'functions'), '');
asserts.istrue(all(ismember({'nelson.compiler.BuildOptions', ...
  'nelson.compiler.BuildResult', 'nelson.compiler.build', 'nelson.compiler.analyze'}, names)));
names{end + 1} = 'compiler_standalone_tutorial';
names{end + 1} = 'compiler_embedded_data_tutorial';
names{end + 1} = 'compiler_build_tutorial';
names{end + 1} = 'compiler_installer_tutorial';
names{end + 1} = 'compiler_linux_installer_tutorial';
names{end + 1} = 'compiler_runtime_tutorial';
names{end + 1} = 'compiler_linux_runtime_tutorial';
names{end + 1} = 'compiler_project_tutorial';
asserts.istrue(all(ismember({'compiler.build.standaloneApplication', ...
  'compiler.build.standaloneWindowsApplication', 'compiler.build.StandaloneApplicationOptions', ...
  'compiler.build.Results', 'compiler.runtime.Dependencies', 'compiler.runtime.customInstaller'}, names)));
for language = {'en_US', 'fr_FR'}
  source = fullfile(root, 'help', language{1}, 'xml');
  destination = fullfile(directory, language{1});
  mkdir(destination);
  asserts.istrue(xmldocchecker(source));
  for k = 1:numel(names)
    filename = fullfile(source, [names{k}, '.xml']);
    asserts.istrue(isfile(filename), filename);
    doc = xmlread(filename, 'AllowDoctype', false);
    keyword = doc.getElementsByTagName('keyword');
    asserts.isequal(double(keyword.getLength()), 1);
    asserts.isequal(char(keyword.item(0).getTextContent()), names{k});
    aliases = doc.getElementsByTagName('keyword_alias');
    asserts.isequal(double(aliases.getLength()), 1);
    asserts.isequal(char(aliases.item(0).getTextContent()), names{k});
  end
  [status, message] = xmldoctohtml(source, destination, 'compiler', true);
  asserts.istrue(status, message);
  index = fileread(fullfile(destination, 'index.html'));
  for k = 1:numel(names)
    filename = [names{k}, '.html'];
    asserts.istrue(isfile(fullfile(destination, filename)), filename);
    asserts.istrue(contains(index, filename), filename);
  end
  options = fileread(fullfile(destination, 'nelson.compiler.BuildOptions.html'));
  optionNames = properties(ncc('options', fullfile(root, 'examples', 'standalone', 'app_entry.m')));
  for k = 1:numel(optionNames)
    asserts.istrue(contains(options, optionNames{k}), optionNames{k});
  end
  options = fileread(fullfile(destination, 'compiler.build.StandaloneApplicationOptions.html'));
  publicOptions = compiler.build.StandaloneApplicationOptions(fullfile(root, 'examples', 'standalone', 'app_entry.m'));
  optionNames = properties(publicOptions);
  for k = 1:numel(optionNames)
    asserts.istrue(contains(options, optionNames{k}), optionNames{k});
  end
  tutorial = fileread(fullfile(destination, 'compiler_standalone_tutorial.html'));
  installerHelp = fileread(fullfile(destination, 'compiler.package.InstallerOptions.html'));
  installerOptions = compiler.package.InstallerOptions('ApplicationName', 'HelpTest');
  installerNames = properties(installerOptions);
  for k = 1:numel(installerNames)
    asserts.istrue(contains(installerHelp, installerNames{k}), installerNames{k});
  end
  for name = {'compiler.build.standaloneApplication', 'compiler.build.standaloneWindowsApplication', ...
    'compiler.build.Results', 'compiler.runtime.Dependencies', 'compiler_build_tutorial'}
    page = fileread(fullfile(destination, [name{1}, '.html']));
    asserts.istrue(contains(page, 'buildresult.json'), name{1});
  end
  for text = {'TUTORIAL_RESULT=30', 'NELSONC_RUNTIME_ROOT', 'AdditionalFiles', ...
    'NoConsole', 'RuntimePlan', 'ctfroot', 'isdeployed'}
    asserts.istrue(contains(tutorial, text{1}), text{1});
  end
end
clear cleanup;

function names = public_functions(directory, prefix)
  names = {};
  files = dir(directory);
  for k = 1:numel(files)
    name = files(k).name;
    if files(k).isdir
      if startsWith(name, '+')
        names = [names, public_functions(fullfile(directory, name), [prefix, name(2:end), '.'])];
      end
    elseif endsWith(name, '.m') && ~startsWith(name, '__')
      names{end + 1} = [prefix, name(1:end - 2)];
    end
  end
end
