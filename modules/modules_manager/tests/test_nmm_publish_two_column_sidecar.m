%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% A ".sha256" sidecar written in the "sha256sum" two-column "<hex>  <name>"
% layout must not break publishing: only the hash is compared.
skip_testsuite(iswasm(), 'Registry publishing requires a writable native registry.');
testRoot = [tempdir(), 'nmm_publish_two_column_sidecar/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
registryFile = [testRoot, 'registry.json'];
packageName = 'module_skeleton_basic-all-3.0.0.nmz';
sourcePackage = [modulepath('modules_manager', 'tests'), '/', packageName];
packageFile = [testRoot, packageName];
copyfile(sourcePackage, packageFile, 'f');
previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousSigningKey = getenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
previousPublicKey = getenv('NELSON_NMM_REGISTRY_PUBLIC_KEY');
%=============================================================================
try
  setenv('NELSON_NMM_REGISTRY', registryFile);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', '');
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', '');
  % two-column sidecar, as produced by the sha256sum tool
  checksum = sha256(packageFile, '-file');
  filewrite([packageFile, '.sha256'], [checksum, '  ', packageName]);
  report = nmm('publish', packageFile);
  asserts.istrue(report.published);
  decoded = jsondecode(registryFile, '-file');
  asserts.isequal(numel(decoded.packages), 1);
catch exception
  restoreEnvironment(previousRegistry, previousSigningKey, previousPublicKey);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
  rethrow(exception);
end
restoreEnvironment(previousRegistry, previousSigningKey, previousPublicKey);
if isdir(testRoot)
  rmdir(testRoot, 's');
end
%=============================================================================
function restoreEnvironment(previousRegistry, previousSigningKey, previousPublicKey)
  setenv('NELSON_NMM_REGISTRY', previousRegistry);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', previousSigningKey);
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', previousPublicKey);
end
%=============================================================================
