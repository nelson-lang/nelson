%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [localFile, info] = nmm_registry_fetch(source)
  % Returns a local, verified copy of the configured registry.
  %
  % Local file: the ".sha256" sidecar is mandatory and the ".sig" sidecar,
  % when present, must carry a valid signature from a trusted key.
  %
  % Remote source (http, https, file): "registry.json" and "registry.json.sig"
  % are fetched as raw bytes, the signature is verified BEFORE the JSON is
  % parsed, then the pair is stored in the nmm cache. When the fetch fails and
  % a previously verified copy exists in the cache, that copy is re-verified
  % and used (offline mode). An unsigned remote registry is refused unless
  % NELSON_NMM_REGISTRY_ALLOW_UNSIGNED=1.
  %
  % NELSON_NMM_REGISTRY_REUSE=1 lets a caller issuing several registry queries
  % in a row (the package manager window refreshing) download the registry
  % once: a remote registry already fetched by this process is then served
  % from its cached copy, re-verified like an offline copy, without network.
  info = struct();
  info.source = source;
  info.remote = nmm_is_remote_registry(source);
  info.signature_present = false;
  info.verified = false;
  info.public_key = '';
  info.offline = false;
  info.cached_file = '';
  info.signature_file = [source, '.sig'];
  if info.remote
    [localFile, info] = fetchRemote(source, info);
  else
    [localFile, info] = fetchLocal(source, info);
  end
end
%=============================================================================
function [localFile, info] = fetchLocal(source, info)
  if ~isfile(source)
    nmm_error('RegistryFileMissing', 'Registry file is missing: %s', source);
  end
  verifyChecksum(source);
  signature = nmm_registry_signature('verify-if-present', source);
  info.signature_present = signature.present;
  info.verified = signature.ok;
  info.public_key = signature.public_key;
  localFile = source;
end
%=============================================================================
function [localFile, info] = fetchRemote(source, info)
  cacheDir = registryCacheDir(source);
  cachedFile = [cacheDir, 'registry.json'];
  if reuseRequested() && fetchedSources('has', source) && isfile(cachedFile)
    info = verifyFetchedRegistry(cachedFile, source, info);
    info.cached_file = cachedFile;
    localFile = cachedFile;
    return
  end
  % Short-lived on-disk cache: an http(s) registry downloaded moments ago is
  % reused (re-verified, no network round trip) so a burst of separate CLI
  % processes (nmm search, then nmm install) fetches it once. Unlike the opt-in
  % in-process reuse above, this survives across processes and is on by default.
  % NELSON_NMM_REGISTRY_CACHE_TTL (seconds, 0 disables) overrides the 60 s
  % default. file:// stays always fresh (local, and tests rewrite it in place).
  if isNetworkRegistry(source) && isFreshRegistryCache(cachedFile)
    try
      info = verifyFetchedRegistry(cachedFile, source, info);
      info.cached_file = cachedFile;
      localFile = cachedFile;
      return
    catch
      % a stale or unusable cached copy falls through to a fresh fetch
    end
  end
  stagingDir = [tempdir(), 'nmm_registry_fetch_', shortHash(source), '/'];
  cleanupDirectory(stagingDir);
  mkdir(stagingDir);
  stagedFile = [stagingDir, 'registry.json'];
  stagedSignature = [stagedFile, '.sig'];
  fetchUrl = fileUrlForFetch(source);
  try
    websave(stagedFile, fetchUrl, nmm_web_options());
  catch e
    cleanupDirectory(stagingDir);
    if isfile(cachedFile)
      [localFile, info] = useOfflineCopy(cachedFile, info, e.message);
      return
    end
    nmm_error('RegistryFetchFailed', 'Registry cannot be fetched: %s (%s)', source, e.message);
  end
  try
    websave(stagedSignature, [fetchUrl, '.sig'], nmm_web_options());
  catch
    if isfile(stagedSignature)
      rmfile(stagedSignature);
    end
  end
  try
    info = verifyFetchedRegistry(stagedFile, source, info);
  catch e
    cleanupDirectory(stagingDir);
    rethrow(e);
  end
  if ~isdir(cacheDir)
    mkdir(cacheDir);
  end
  copyfile(stagedFile, cachedFile);
  cachedSignature = [cachedFile, '.sig'];
  if isfile(stagedSignature)
    copyfile(stagedSignature, cachedSignature);
  elseif isfile(cachedSignature)
    rmfile(cachedSignature);
  end
  filewrite([cacheDir, 'source.txt'], source);
  cleanupDirectory(stagingDir);
  fetchedSources('add', source);
  info.cached_file = cachedFile;
  localFile = cachedFile;
end
%=============================================================================
function info = verifyFetchedRegistry(registryFile, source, info)
  signatureFile = [registryFile, '.sig'];
  if isfile(signatureFile)
    signature = nmm_registry_signature('verify', registryFile);
    info.signature_present = true;
    info.verified = signature.ok;
    info.public_key = signature.public_key;
    return
  end
  if ~allowUnsigned()
    nmm_error('RegistrySignatureMissing', 'Registry signature file is missing.');
  end
  warnUnsignedOnce(source);
end
%=============================================================================
function [localFile, info] = useOfflineCopy(cachedFile, info, reason)
  info = verifyFetchedRegistry(cachedFile, info.source, info);
  info.offline = true;
  info.cached_file = cachedFile;
  localFile = cachedFile;
  warning('Nelson:nmm:RegistryOffline', 'Registry cannot be fetched (%s): using the last verified copy.', reason);
end
%=============================================================================
function tf = reuseRequested()
  tf = strcmp(strtrim(getenv('NELSON_NMM_REGISTRY_REUSE')), '1');
end
%=============================================================================
function tf = isNetworkRegistry(source)
  tf = ischar(source) && (startsWith(source, 'http://') || startsWith(source, 'https://'));
end
%=============================================================================
function tf = isFreshRegistryCache(cachedFile)
  ttl = registryCacheTtlSeconds();
  if ttl <= 0 || ~isfile(cachedFile)
    tf = false;
    return
  end
  entry = dir(cachedFile);
  if isempty(entry) || ~isfield(entry, 'datenum')
    tf = false;
    return
  end
  ageSeconds = (now - entry.datenum) * 86400;
  tf = ageSeconds >= 0 && ageSeconds < ttl;
end
%=============================================================================
function ttl = registryCacheTtlSeconds()
  value = strtrim(getenv('NELSON_NMM_REGISTRY_CACHE_TTL'));
  if isempty(value)
    ttl = 60;
    return
  end
  n = str2double(value);
  if isnan(n) || n < 0
    ttl = 60;
  else
    ttl = n;
  end
end
%=============================================================================
function tf = fetchedSources(action, source)
  % Remote registries downloaded (and verified) by this process.
  persistent sources
  if isempty(sources)
    sources = {};
  end
  tf = any(strcmp(sources, source));
  if strcmp(action, 'add') && ~tf
    sources{end + 1} = source;
  end
end
%=============================================================================
function tf = allowUnsigned()
  tf = strcmp(strtrim(getenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED')), '1');
end
%=============================================================================
function warnUnsignedOnce(source)
  persistent warnedSources
  if isempty(warnedSources)
    warnedSources = {};
  end
  if any(strcmp(warnedSources, source))
    return
  end
  warnedSources{end + 1} = source;
  warning('Nelson:nmm:RegistryUnsigned', 'Registry is not signed (NELSON_NMM_REGISTRY_ALLOW_UNSIGNED=1): %s', source);
end
%=============================================================================
function verifyChecksum(registryFile)
  checksumFile = [registryFile, '.sha256'];
  if ~isfile(checksumFile)
    nmm_error('RegistryChecksumMissing', 'Registry checksum file is missing.');
  end
  expected = strtrim(fileread(checksumFile));
  separator = strfind(expected, ' ');
  if ~isempty(separator)
    expected = expected(1:separator(1) - 1);
  end
  if ~strcmpi(expected, sha256(registryFile, '-file'))
    nmm_error('RegistryChecksumMismatch', 'Registry checksum does not match.');
  end
end
%=============================================================================
function cacheDir = registryCacheDir(source)
  cacheDir = [nmm_cache('dir'), 'registry/', sha256(source, '-string'), '/'];
end
%=============================================================================
function h = shortHash(source)
  h = sha256(source, '-string');
  h = h(1:16);
end
%=============================================================================
function url = fileUrlForFetch(source)
  % A file:// registry URL may carry a filesystem path with spaces or other
  % characters that are illegal in a URL (for example an install directory such
  % as "Nelson-2.0.0.0 (64 bits)"). Percent-encode the path so websave/curl
  % accepts it; http(s) sources are assumed to be already well-formed.
  url = source;
  prefix = 'file://';
  if ~startsWith(source, prefix)
    return
  end
  url = [prefix, encodeUrlPath(extractAfter(source, prefix))];
end
%=============================================================================
function out = encodeUrlPath(p)
  % Percent-encode a URL path, preserving the separators '/' and ':' and leaving
  % already-encoded '%XX' triplets untouched. Non-ASCII bytes are encoded from
  % their UTF-8 representation.
  bytes = unicode2native(p, 'UTF-8');
  pieces = cell(1, numel(bytes));
  for k = 1:numel(bytes)
    b = bytes(k);
    if b >= 48 && b <= 57 ...                 % 0-9
        || b >= 65 && b <= 90 ...             % A-Z
        || b >= 97 && b <= 122 ...            % a-z
        || b == 45 || b == 46 || b == 95 || b == 126 ... % - . _ ~
        || b == 47 || b == 58 || b == 37      % / : %
      pieces{k} = char(b);
    else
      pieces{k} = sprintf('%%%02X', b);
    end
  end
  out = [pieces{:}];
end
%=============================================================================
function cleanupDirectory(directory)
  if isdir(directory)
    rmdir(directory, 's');
  end
end
%=============================================================================
