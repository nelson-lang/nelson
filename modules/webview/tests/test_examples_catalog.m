%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The unified examples catalog (module manifests + registered providers) is
% available without opening any window: the web desktop and NFlow read it.
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('examples catalog tests require GUI mode.'));
%=============================================================================
catalog = jsondecode(__webview__('examplesCatalog'));
asserts.istrue(isstruct(catalog));
asserts.istrue(isfield(catalog, 'groups'));
asserts.istrue(isfield(catalog, 'diagnostics'));
groups = catalog.groups;
if isstruct(groups)
  groups = num2cell(groups);
end
asserts.istrue(numel(groups) > 0);
%=============================================================================
% Every entry carries the common schema: kind, title, description, tags,
% requirements and either a file or a command.
kinds = {};
sources = {};
for i = 1:numel(groups)
  group = groups{i};
  asserts.istrue(ischar(group.name) && ~isempty(group.name));
  asserts.istrue(ischar(group.title) && ~isempty(group.title));
  asserts.istrue(any(strcmp(group.source, {'nelson', 'external', 'nflow'})));
  sources{end + 1} = group.source;
  examples = group.examples;
  if isstruct(examples)
    examples = num2cell(examples);
  end
  for j = 1:numel(examples)
    example = examples{j};
    asserts.istrue(any(strcmp(example.kind, {'script', 'command', 'model'})));
    asserts.istrue(ischar(example.title) && ~isempty(example.title));
    asserts.istrue(isfield(example, 'description'));
    asserts.istrue(isfield(example, 'tags'));
    asserts.istrue(isfield(example, 'requirements'));
    if strcmp(example.kind, 'command')
      asserts.istrue(ischar(example.command) && ~isempty(example.command));
    else
      asserts.istrue(isfile(example.file));
    end
    if strcmp(example.kind, 'model')
      asserts.istrue(endsWith(example.file, '.nflow'));
    end
    kinds{end + 1} = example.kind;
  end
end
asserts.istrue(any(strcmp(sources, 'nelson')));
asserts.istrue(any(strcmp(kinds, 'script')));
%=============================================================================
% Providers are resolved lazily: an unavailable library is reported in the
% diagnostics instead of breaking the catalog, and can be unregistered.
missingLibrary = [tempdir(), 'libnlsMissingExamplesProvider', getdynlibext()];
__webview__('registerExamplesProvider', missingLibrary, 'nlsMissingProvider');
catalog = jsondecode(__webview__('examplesCatalog'));
diagnostics = catalog.diagnostics;
if isstruct(diagnostics)
  diagnostics = num2cell(diagnostics);
end
found = false;
for i = 1:numel(diagnostics)
  if contains(diagnostics{i}.module, 'libnlsMissingExamplesProvider')
    found = true;
  end
end
asserts.istrue(found);
asserts.istrue(__webview__('unregisterExamplesProvider', missingLibrary));
asserts.isfalse(__webview__('unregisterExamplesProvider', missingLibrary));
catalog = jsondecode(__webview__('examplesCatalog'));
diagnostics = catalog.diagnostics;
if isstruct(diagnostics)
  diagnostics = num2cell(diagnostics);
end
for i = 1:numel(diagnostics)
  asserts.isfalse(contains(diagnostics{i}.module, 'libnlsMissingExamplesProvider'));
end
%=============================================================================
