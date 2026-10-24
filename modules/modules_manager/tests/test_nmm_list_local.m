%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
check_local_list();

function check_local_list()
  work = tempname(); mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  old = getenv('NELSON_EXTERNAL_MODULES_PATH');
  restorePath = onCleanup(@() setenv('NELSON_EXTERNAL_MODULES_PATH', old));
  setenv('NELSON_EXTERNAL_MODULES_PATH', work);
  % This read-only operation also runs in builds without network/archive modules.
  asserts.isequal(nmm('list'), struct([]));
  asserts.isequal(jsondecode(nmm('list', '-json')), cell(0, 1));
  data = struct('local_package', struct('path', work, 'version', '1.0.0'));
  filename = fullfile(work, 'modules.json');
  filewrite(filename, jsonencode(data));
  before = sha256(filename, '-file');
  asserts.isequal(nmm('list'), data);
  asserts.isequal(nmm('list', '-quiet'), data);
  asserts.isequal(jsondecode(nmm('list', '-json', '-quiet')), data);
  asserts.isequal(sha256(filename, '-file'), before);
  rejects(@() nmm('list', 'unexpected'), 'Nelson:nmm:WrongNumberOfInputArguments');
  if ~ismodule('webtools')
    rejects_any(@() nmm('install', work), ...
      {'Nelson:nmm:RequiredModuleMissing', 'Nelson:nmm:ModuleDirectoryExpectedValidModuleRepository'});
  elseif ~ismodule('file_archiver')
    rejects(@() nmm('install', work), 'Nelson:nmm:RequiredModuleMissingNmmRequiredFile');
  end
  filewrite(filename, '{invalid');
  failed = false; try; nmm('list'); catch; failed = true; end
  asserts.istrue(failed);
  clear restorePath cleanup;
end

function rejects(callback, identifier)
  caught = ''; try; callback(); catch ex; caught = ex.identifier; end
  asserts.isequal(caught, identifier);
end

function rejects_any(callback, identifiers)
  caught = ''; try; callback(); catch ex; caught = ex.identifier; end
  asserts.istrue(any(strcmp(caught, identifiers)));
end
