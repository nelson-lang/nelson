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
% Ed25519 signature of the package registry: local and remote (file://)
% sources, trusted keys, tampering, unsigned opt-out, offline cache.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Registry signature cache tests require a writable native cache.');
testRoot = [tempdir(), 'nmm_registry_signature/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
registryFile = [testRoot, 'registry.json'];
signatureFile = [registryFile, '.sig'];
registryUrl = ['file:///', strrep(registryFile, '\', '/')];
cacheDir = [nmm('cache', 'dir'), 'registry/', sha256(registryUrl, '-string'), '/'];
previousRegistry = getenv('NELSON_NMM_REGISTRY');
previousSigningKey = getenv('NELSON_NMM_REGISTRY_SIGNING_KEY');
previousPublicKey = getenv('NELSON_NMM_REGISTRY_PUBLIC_KEY');
previousAllowUnsigned = getenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED');
% RFC 8032 test vector 3 seed (a public test key, never use it for real)
seed = 'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7';
[~, publicKey] = crypto.ed25519.sign(uint8([]), seed);
%=============================================================================
try
  setenv('NELSON_NMM_REGISTRY', registryFile);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', '');
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', '');
  setenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED', '');
  registry = [
    "{";
    "  ""packages"": [";
    "    {";
    "      ""name"": ""signal_tools"",";
    "      ""version"": ""1.0.0"",";
    "      ""summary"": ""Signal helpers"",";
    "      ""platforms"": [""all""],";
    "      ""nelson"": "">=1.0.0"",";
    "      ""dependencies"": {},";
    "      ""checksum"": ""abc"",";
    "      ""signature"": """",";
    "      ""source"": ""missing-source""";
    "    }";
    "  ]";
    "}"
  ];
  filewrite(registryFile, registry);
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));
  % local registry without signature: accepted (development registry)
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  report = nmm('registry', 'check');
  asserts.istrue(report.ok);
  asserts.istrue(report.is_local);
  asserts.isfalse(report.signature_ok);
  % Nelson ships its own trusted key(s), and a rotation may add one more, so the
  % built-in list is the baseline the configured keys below are counted against.
  builtinTrustedKeys = report.trusted_keys;
  asserts.istrue(builtinTrustedKeys >= 1);
  %===========================================================================
  % signed sidecar written by hand: documents the format contract
  signature = crypto.ed25519.sign(registryFile, seed, '-file');
  signatureDocument = sprintf(['{"format": "nmm-registry-signature", "version": 1, "algorithm": "ed25519", ', ...
    '"file": "registry.json", "sha256": "%s", "signatures": [{"public_key": "%s", "signature": "%s"}]}'], ...
    sha256(registryFile, '-file'), publicKey, signature);
  filewrite(signatureFile, signatureDocument);
  % key not trusted yet
  asserts.checkerror('nmm(''search'', ''signal'')', sprintf(_('Registry signature key is not trusted: %s'), publicKey), 'Nelson:nmm:RegistrySignatureUntrustedKey');
  report = nmm('registry', 'check');
  asserts.isfalse(report.ok);
  % trust the key through the environment
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', upper(publicKey));
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  report = nmm('registry', 'check');
  asserts.istrue(report.ok);
  asserts.istrue(report.checksum_ok);
  asserts.istrue(report.signature_ok);
  asserts.isequal(report.signature_key, publicKey);
  asserts.isequal(report.trusted_keys, builtinTrustedKeys + 1);
  configReport = nmm('config');
  asserts.isequal(configReport.trusted_keys, builtinTrustedKeys + 1);
  asserts.isfalse(configReport.allow_unsigned_registry);
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', 'zz');
  asserts.checkerror('nmm(''config'')', sprintf(_('Registry public key must be 64 hexadecimal characters: %s'), 'zz'), 'Nelson:nmm:RegistryPublicKeyInvalid');
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', upper(publicKey));
  %===========================================================================
  % tampered signature (valid hex, wrong value)
  otherSignature = crypto.ed25519.sign('other content', seed);
  filewrite(signatureFile, strrep(signatureDocument, signature, otherSignature));
  asserts.checkerror('nmm(''search'', ''signal'')', _('Registry signature does not match.'), 'Nelson:nmm:RegistrySignatureMismatch');
  % tampered registry content: the mandatory checksum sidecar catches it first
  filewrite(signatureFile, signatureDocument);
  filewrite(registryFile, strrep(registry, 'abc', 'abd'));
  asserts.checkerror('nmm(''search'', ''signal'')', _('Registry checksum does not match.'), 'Nelson:nmm:RegistryChecksumMismatch');
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));
  asserts.checkerror('nmm(''search'', ''signal'')', _('Registry signature does not match.'), 'Nelson:nmm:RegistrySignatureMismatch');
  filewrite(registryFile, registry);
  filewrite([registryFile, '.sha256'], sha256(registryFile, '-file'));
  % malformed sidecars
  invalidMessage = sprintf(_('Registry signature file is not valid: %s'), signatureFile);
  filewrite(signatureFile, '{"format": "something-else", "version": 1, "algorithm": "ed25519", "signatures": []}');
  asserts.checkerror('nmm(''search'', ''signal'')', invalidMessage, 'Nelson:nmm:RegistrySignatureFormatInvalid');
  filewrite(signatureFile, 'not json at all');
  asserts.checkerror('nmm(''search'', ''signal'')', invalidMessage, 'Nelson:nmm:RegistrySignatureFormatInvalid');
  filewrite(signatureFile, strrep(signatureDocument, publicKey, publicKey(1:10)));
  asserts.checkerror('nmm(''search'', ''signal'')', invalidMessage, 'Nelson:nmm:RegistrySignatureFormatInvalid');
  filewrite(signatureFile, signatureDocument);
  %===========================================================================
  % a configured signing seed trusts its own key and nmm publish writes the sidecar
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', '');
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', seed);
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  configReport = nmm('config');
  asserts.istrue(configReport.signing_key_configured);
  asserts.isequal(configReport.trusted_keys, builtinTrustedKeys + 1);
  packageFile = [modulepath('modules_manager', 'tests'), '/module_skeleton_basic-all-3.0.0.nmz'];
  publishReport = nmm('publish', packageFile);
  asserts.istrue(publishReport.published);
  asserts.istrue(isfile(signatureFile));
  written = jsondecode(signatureFile, '-file');
  asserts.isequal(written.format, 'nmm-registry-signature');
  asserts.isequal(written.version, 1);
  asserts.isequal(written.algorithm, 'ed25519');
  asserts.isequal(written.file, 'registry.json');
  asserts.isequal(written.sha256, sha256(registryFile, '-file'));
  asserts.isequal(written.signatures(1).public_key, publicKey);
  asserts.istrue(crypto.ed25519.verify(registryFile, written.signatures(1).signature, publicKey, '-file'));
  publishedSignatureDocument = fileread(signatureFile);
  result = nmm('search', 'module_skeleton');
  asserts.isequal(length(result), 1);
  report = nmm('registry', 'check');
  asserts.istrue(report.ok);
  asserts.istrue(report.signature_ok);
  asserts.isequal(report.signature_key, publicKey);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', 'zz');
  asserts.checkerror('nmm(''search'', ''signal'')', _('Registry signing key must be 64 hexadecimal characters (Ed25519 seed).'), 'Nelson:nmm:RegistrySigningKeyInvalid');
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', '');
  %===========================================================================
  % remote registry (file:// goes through websave like http): verified copy cached
  if isdir(cacheDir)
    rmdir(cacheDir, 's');
  end
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', publicKey);
  setenv('NELSON_NMM_REGISTRY', registryUrl);
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  asserts.istrue(isfile([cacheDir, 'registry.json']));
  asserts.istrue(isfile([cacheDir, 'registry.json.sig']));
  asserts.isequal(sha256([cacheDir, 'registry.json'], '-file'), sha256(registryFile, '-file'));
  report = nmm('registry', 'check');
  asserts.istrue(report.ok);
  asserts.isfalse(report.is_local);
  asserts.istrue(report.signature_ok);
  asserts.isequal(report.signature_key, publicKey);
  asserts.isfalse(report.offline);
  asserts.isequal(report.cached_file, [cacheDir, 'registry.json']);
  stats = nmm('registry', 'stats');
  asserts.isfalse(stats.is_local);
  % unsigned remote registry: refused unless explicitly allowed
  rmfile(signatureFile);
  asserts.checkerror('nmm(''search'', ''signal'')', _('Registry signature file is missing.'), 'Nelson:nmm:RegistrySignatureMissing');
  asserts.istrue(isfile([cacheDir, 'registry.json.sig']));
  setenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED', '1');
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  asserts.isfalse(isfile([cacheDir, 'registry.json.sig']));
  report = nmm('registry', 'check');
  asserts.isfalse(report.ok);
  asserts.isfalse(report.signature_ok);
  asserts.istrue(any(strcmp(report.issues, _('Registry signature file is missing.'))));
  setenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED', '');
  filewrite(signatureFile, publishedSignatureDocument);
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  % source unreachable: the last verified copy is used (offline), still verified
  movefile(registryFile, [registryFile, '.moved']);
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
  report = nmm('registry', 'check');
  asserts.istrue(report.ok);
  asserts.istrue(report.offline);
  asserts.istrue(report.signature_ok);
  rmdir(cacheDir, 's');
  try
    nmm('search', 'signal');
    asserts.istrue(false);
  catch e
    asserts.isequal(e.identifier, 'Nelson:nmm:RegistryFetchFailed');
  end
  movefile([registryFile, '.moved'], registryFile);
  result = nmm('search', 'signal');
  asserts.isequal(length(result), 1);
catch e
  cleanupTest(testRoot, cacheDir, previousRegistry, previousSigningKey, previousPublicKey, previousAllowUnsigned);
  rethrow(e);
end
cleanupTest(testRoot, cacheDir, previousRegistry, previousSigningKey, previousPublicKey, previousAllowUnsigned);
%=============================================================================
function cleanupTest(testRoot, cacheDir, previousRegistry, previousSigningKey, previousPublicKey, previousAllowUnsigned)
  setenv('NELSON_NMM_REGISTRY', previousRegistry);
  setenv('NELSON_NMM_REGISTRY_SIGNING_KEY', previousSigningKey);
  setenv('NELSON_NMM_REGISTRY_PUBLIC_KEY', previousPublicKey);
  setenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED', previousAllowUnsigned);
  if isdir(cacheDir)
    rmdir(cacheDir, 's');
  end
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
end
%=============================================================================
