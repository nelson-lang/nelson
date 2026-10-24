%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = nmm_registry_signature(action, registryFile)
  % Ed25519 signature of a registry file, carried by the JSON sidecar
  % "<registry>.sig" (format described in writeRegistrySignature).
  switch action
    case 'verify'
      out = verifyRegistrySignature(registryFile, true);
    case 'verify-if-present'
      out = verifyRegistrySignature(registryFile, false);
    case 'write'
      out = writeRegistrySignature(registryFile);
    case 'trusted-keys'
      out = trustedKeys();
    otherwise
      nmm_error('UnknownRegistrySignatureAction', 'Unknown registry signature action.');
  end
end
%=============================================================================
function info = verifyRegistrySignature(registryFile, required)
  info = struct();
  info.signature_file = [registryFile, '.sig'];
  info.present = isfile(info.signature_file);
  info.ok = false;
  info.public_key = '';
  if ~info.present
    if required
      nmm_error('RegistrySignatureMissing', 'Registry signature file is missing.');
    end
    return
  end
  signatures = readSignatureFile(info.signature_file);
  keys = trustedKeys();
  trustedFound = false;
  seen = {};
  for k = 1:length(signatures)
    publicKey = signatures(k).public_key;
    seen{end + 1} = publicKey;
    if ~any(strcmp(keys, publicKey))
      continue
    end
    trustedFound = true;
    if crypto.ed25519.verify(registryFile, signatures(k).signature, publicKey, '-file')
      info.ok = true;
      info.public_key = publicKey;
      return
    end
  end
  if ~trustedFound
    nmm_error('RegistrySignatureUntrustedKey', 'Registry signature key is not trusted: %s', strjoin(seen, ', '));
  end
  nmm_error('RegistrySignatureMismatch', 'Registry signature does not match.');
end
%=============================================================================
function signatures = readSignatureFile(signatureFile)
  try
    document = jsondecode(signatureFile, '-file');
  catch
    nmm_error('RegistrySignatureFormatInvalid', 'Registry signature file is not valid: %s', signatureFile);
  end
  valid = isstruct(document) && isfield(document, 'format') && isfield(document, 'version') ...
    && isfield(document, 'algorithm') && isfield(document, 'signatures') ...
    && ischar(document.format) && strcmp(document.format, 'nmm-registry-signature') ...
    && isnumeric(document.version) && isscalar(document.version) && document.version == 1 ...
    && ischar(document.algorithm) && strcmp(document.algorithm, 'ed25519');
  if ~valid
    nmm_error('RegistrySignatureFormatInvalid', 'Registry signature file is not valid: %s', signatureFile);
  end
  entries = document.signatures;
  if iscell(entries)
    entries = [entries{:}];
  end
  if ~isstruct(entries) || isempty(entries) || ~isfield(entries, 'public_key') || ~isfield(entries, 'signature')
    nmm_error('RegistrySignatureFormatInvalid', 'Registry signature file is not valid: %s', signatureFile);
  end
  signatures = struct([]);
  for k = 1:length(entries)
    publicKey = entries(k).public_key;
    signature = entries(k).signature;
    if ~isHexString(publicKey, 64) || ~isHexString(signature, 128)
      nmm_error('RegistrySignatureFormatInvalid', 'Registry signature file is not valid: %s', signatureFile);
    end
    entry = struct('public_key', lower(publicKey), 'signature', lower(signature));
    if isempty(signatures)
      signatures = entry;
    else
      signatures(end + 1) = entry;
    end
  end
end
%=============================================================================
function info = writeRegistrySignature(registryFile)
  % Sidecar format (hexadecimal everywhere, pure Ed25519 over the exact bytes
  % of the registry file, several signatures allowed for key rotation):
  % {
  %   "format": "nmm-registry-signature", "version": 1, "algorithm": "ed25519",
  %   "file": "registry.json", "sha256": "<hex>",
  %   "signatures": [ { "public_key": "<hex 64>", "signature": "<hex 128>" } ]
  % }
  info = struct();
  info.signature_file = [registryFile, '.sig'];
  info.written = false;
  info.public_key = '';
  seed = signingSeed();
  if isempty(seed)
    return
  end
  [signature, publicKey] = crypto.ed25519.sign(registryFile, seed, '-file');
  [~, name, ext] = fileparts(registryFile);
  document = struct();
  document.format = 'nmm-registry-signature';
  document.version = 1;
  document.algorithm = 'ed25519';
  document.file = [name, ext];
  document.sha256 = sha256(registryFile, '-file');
  document.signatures = {struct('public_key', publicKey, 'signature', signature)};
  filewrite(info.signature_file, jsonprettyprint(jsonencode(document)));
  info.written = true;
  info.public_key = publicKey;
end
%=============================================================================
function keys = trustedKeys()
  % official keys + NELSON_NMM_REGISTRY_PUBLIC_KEY (';' separated) + the key
  % derived from the configured signing seed (a signer trusts itself).
  keys = nmm_registry_trusted_keys();
  extra = getenv('NELSON_NMM_REGISTRY_PUBLIC_KEY');
  if ~isempty(extra)
    parts = strsplit(extra, ';');
    for k = 1:length(parts)
      key = strtrim(parts{k});
      if isempty(key)
        continue
      end
      if ~isHexString(key, 64)
        nmm_error('RegistryPublicKeyInvalid', 'Registry public key must be 64 hexadecimal characters: %s', key);
      end
      keys{end + 1} = key;
    end
  end
  seed = signingSeed();
  if ~isempty(seed)
    [~, publicKey] = crypto.ed25519.sign(uint8([]), seed);
    keys{end + 1} = publicKey;
  end
  keys = unique(lower(keys));
  if ~iscell(keys)
    keys = {};
  end
end
%=============================================================================
function seed = signingSeed()
  seed = strtrim(getenv('NELSON_NMM_REGISTRY_SIGNING_KEY'));
  if isempty(seed)
    seed = '';
    return
  end
  if ~isHexString(seed, 64)
    nmm_error('RegistrySigningKeyInvalid', 'Registry signing key must be 64 hexadecimal characters (Ed25519 seed).');
  end
end
%=============================================================================
function tf = isHexString(value, expectedLength)
  tf = ischar(value) && isrow(value) && length(value) == expectedLength ...
    && all(ismember(lower(value), '0123456789abcdef'));
end
%=============================================================================
