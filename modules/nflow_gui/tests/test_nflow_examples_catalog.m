%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NFlow registers an examples provider at startup: its models and scripts
% join the unified examples catalog shared by demo(), the web desktop and
% the NFlow example browser.
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('examples catalog tests require GUI mode.'));
%=============================================================================
catalog = jsondecode(__webview__('examplesCatalog'));
groups = catalog.groups;
if isstruct(groups)
  groups = num2cell(groups);
end
nflowGroups = {};
for i = 1:numel(groups)
  if strcmp(groups{i}.source, 'nflow')
    nflowGroups{end + 1} = groups{i};
  end
end
asserts.istrue(numel(nflowGroups) >= 2);
%=============================================================================
% Built-in categories keep their usual order and every model points at a
% .nflow document with its block count.
asserts.isequal(nflowGroups{1}.name, 'nflow:causal');
asserts.isequal(nflowGroups{1}.title, 'Causal');
asserts.isequal(nflowGroups{2}.name, 'nflow:acausal');
models = 0;
scripts = 0;
for i = 1:numel(nflowGroups)
  examples = nflowGroups{i}.examples;
  if isstruct(examples)
    examples = num2cell(examples);
  end
  for j = 1:numel(examples)
    example = examples{j};
    asserts.istrue(isfile(example.file));
    if strcmp(example.kind, 'model')
      asserts.istrue(endsWith(example.file, '.nflow'));
      asserts.istrue(isfield(example, 'blockCount'));
      asserts.istrue(example.blockCount > 0);
      models = models + 1;
    else
      asserts.isequal(example.kind, 'script');
      asserts.istrue(endsWith(example.file, '.m'));
      scripts = scripts + 1;
    end
  end
end
asserts.istrue(models > 10);
asserts.istrue(scripts > 0);
%=============================================================================
% Titles are unique inside a category and sorted.
titles = {};
examples = nflowGroups{1}.examples;
if isstruct(examples)
  examples = num2cell(examples);
end
for j = 1:numel(examples)
  titles{end + 1} = examples{j}.title;
end
asserts.isequal(titles, sort(titles));
asserts.isequal(numel(unique(titles)), numel(titles));
%=============================================================================
