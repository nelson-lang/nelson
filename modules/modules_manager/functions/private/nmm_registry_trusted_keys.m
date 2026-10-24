%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function keys = nmm_registry_trusted_keys()
  % Ed25519 public keys (64 lowercase hexadecimal characters) trusted to sign
  % the official Nelson package registry. Keep the current key and the next
  % key while a rotation is in progress; remove a key only once every
  % registry still served has been re-signed with its successor.
  %
  % Additional keys for private registries come from the environment
  % variable NELSON_NMM_REGISTRY_PUBLIC_KEY (';' separated) and, when a
  % signing seed is configured, from the seed itself: see
  % nmm_registry_signature('trusted-keys').
  keys = {'4113b7e0c4091683ca17524057769d3f1fadf2c22b371f135798dd35fde94924'};
end
%=============================================================================
