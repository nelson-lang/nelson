%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
bodies = {'value = 42;', 'disp(''ready'');', 'disp(sum([1, 2]));'};
for k = 1:numel(bodies)
  name = sprintf('shape_entry_%d', k);
  entry = fullfile(directory, [name, '.m']);
  filewrite(entry, ['function ', name, '(); ', bodies{k}, ' end']);
  encoded = __nelsonc_plan__('link', entry);
  application = jsondecode(encoded);
  asserts.istrue(application.complete);
  asserts.isequal(numel(application.modules), k - 1);
  original = jsondecode(__nelsonc_plan__('runtime', encoded, false));
  roundtrip = jsondecode(__nelsonc_plan__('runtime', jsonencode(application), false));
  asserts.isequal(roundtrip, original);
  asserts.isfalse(any(strcmp(roundtrip.modules, 'compiler')));
end

% The empty list is valid; null, field values and malformed records are not.
invalid = {'true', 'false', 'null', '0', '"engine"', '{}', ...
  '{"complete":false}', '{"name":false}', '{"name":["core"]}', ...
  '[true]', '[{}]', '[[{"name":"core"}]]'};
for k = 1:numel(invalid)
  assert_invalid_plan(['{"modules":', invalid{k}, '}']);
end
for encoded = {'{}', '[]', 'null', 'true'}
  assert_invalid_plan(encoded{1});
end
for encoded = {'{"modules":{"name":"compiler"}}', '{"modules":[{"name":"compiler"}]}'}
  assert_invalid_plan(encoded{1});
end
clear cleanup;

function assert_invalid_plan(encoded)
  identifier = '';
  try
    __nelsonc_plan__('runtime', encoded, false);
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, 'Nelson:nelsonc:invalidPlan');
end
