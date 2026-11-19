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
% Publishing into an empty registry: "packages" is a JSON array even for a
% single entry, the checksum sidecar uses the canonical layout, and a publish
% without a signing key drops a now-stale signature instead of leaving it.
skip_testsuite(iswasm(), 'Registry publishing requires a writable native registry.');
testRoot = [tempdir(), 'nmm_publish_single_package/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
registryFile = [testRoot, 'registry.json'];
checksumFile = [registryFile, '.sha256'];
signatureFile = [registryFile, '.sig'];
packageFile = [modulepath('modules_manager', 'tests'), '/module_skeleton_basic-all-3.0.0.nmz'];
previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousSigningKey = getenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
previousPublicKey = getenv('NELSON_NMM_REGISTRY_PUBLIC_KEY');
% RFC 8032 test vector 3 seed (a public test key, never use it for real)
seed = 'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7';
%=============================================================================
try
  setenv('NELSON_NMM_REGISTRY', registryFile);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', '');
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', '');
  %===========================================================================
  % first package in a fresh registry: "packages" must be an array, not an object
  report = nmm('publish', packageFile);
  asserts.istrue(report.published);
  raw = fileread(registryFile);
  asserts.isfalse(isempty(strfind(raw, '"packages":[')));
  asserts.istrue(isempty(strfind(raw, '"packages":{')));
  decoded = jsondecode(registryFile, '-file');
  asserts.isequal(numel(decoded.packages), 1);
  asserts.isequal(numel(nmm('search', '')), 1);
  %===========================================================================
  % checksum sidecar: canonical "<hex>  registry.json" layout
  asserts.istrue(isfile(checksumFile));
  asserts.isequal(strtrim(fileread(checksumFile)), [sha256(registryFile, '-file'), '  registry.json']);
  %===========================================================================
  % no signature was written because no signing key is configured
  asserts.isfalse(isfile(signatureFile));
  %===========================================================================
  % sign the registry, then re-publish without a key: the signature goes
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', seed);
  nmm('publish', packageFile, '-force');
  asserts.istrue(isfile(signatureFile));
  [~, testPublicKey] = crypto.ed25519.sign(uint8([]), seed);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', '');
  % trust the key so reading the still-signed registry succeeds; the keyless
  % publish then drops the signature it can no longer renew
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', testPublicKey);
  lastwarn('');
  nmm('publish', packageFile, '-force');
  [~, warnId] = lastwarn();
  asserts.isequal(warnId, 'Nelson:nmm:RegistrySignatureNotRenewed');
  asserts.isfalse(isfile(signatureFile));
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
