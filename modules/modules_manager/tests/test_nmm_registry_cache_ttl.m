%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% An http(s) registry fetched moments ago is reused from its on-disk cache
% (re-verified, no network) for NELSON_NMM_REGISTRY_CACHE_TTL seconds (60 by
% default), so a burst of separate CLI processes downloads it once. TTL=0
% disables the fast path. The registry source below is unreachable, so the
% fast path is the only way the check can succeed without going offline.
%=============================================================================
skip_testsuite(~ismodule('webtools') || (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Remote registry cache tests require webtools and a writable native cache.');
% RFC 8032 test vector 3 seed (a public test key, never use it for real)
seed = 'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7';
[~, publicKey] = crypto.ed25519.sign(uint8([]), seed);
source = 'http://127.0.0.1:9/registry.json';
cacheDir = [nmm('cache', 'dir'), 'registry/', sha256(source, '-string'), '/'];
cachedFile = [cacheDir, 'registry.json'];

previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousPublicKey = getenv('NELSON_NMM_REGISTRY_PUBLIC_KEY');
previousTtl = getenv('NELSON_NMM_REGISTRY_CACHE_TTL');
previousReuse = getenv('NELSON_NMM_REGISTRY_REUSE');

if isdir(cacheDir)
  rmdir(cacheDir, 's');
end
mkdir(cacheDir);
registryText = ['{"packages": [{"name": "ttl_fixture", "version": "1.0.0", ', ...
  '"summary": "cache ttl fixture", "platforms": ["all"], "nelson": ">=1.0.0", ', ...
  '"dependencies": {}, "checksum": "abc", "signature": "", "source": "missing"}]}'];
filewrite(cachedFile, registryText);
signature = crypto.ed25519.sign(cachedFile, seed, '-file');
signatureDocument = sprintf(['{"format": "nmm-registry-signature", "version": 1, ', ...
  '"algorithm": "ed25519", "file": "registry.json", "sha256": "%s", ', ...
  '"signatures": [{"public_key": "%s", "signature": "%s"}]}'], ...
  sha256(cachedFile, '-file'), publicKey, signature);
filewrite([cachedFile, '.sig'], signatureDocument);

setenv('NELSON_NMM_REGISTRY', source);
setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', publicKey);
setenv('NELSON_NMM_REGISTRY_REUSE', '');

% fresh cache (default 60 s TTL): served from disk, no network -> not offline
setenv('NELSON_NMM_REGISTRY_CACHE_TTL', '');
report = nmm('registry', 'check');
asserts.istrue(report.ok);
asserts.isfalse(report.offline);
asserts.istrue(report.signature_ok);
result = nmm('search', 'ttl_fixture');
asserts.isequal(length(result), 1);

% TTL disabled: the (unreachable) network is attempted, then the offline copy
% is used, so the same query now reports offline
setenv('NELSON_NMM_REGISTRY_CACHE_TTL', '0');
report = nmm('registry', 'check');
asserts.istrue(report.offline);

setenv('NELSON_NMM_REGISTRY_CACHE_TTL', '');
if isdir(cacheDir)
  rmdir(cacheDir, 's');
end
restoreEnv('NELSON_NMM_REGISTRY', previousRegistry);
restoreEnv('NELSON_NMM_REGISTRY_PUBLIC_KEY', previousPublicKey);
restoreEnv('NELSON_NMM_REGISTRY_CACHE_TTL', previousTtl);
restoreEnv('NELSON_NMM_REGISTRY_REUSE', previousReuse);
%=============================================================================
function restoreEnv(name, value)
  if isempty(value)
    setenv(name);
  else
    setenv(name, value);
  end
end
%=============================================================================
