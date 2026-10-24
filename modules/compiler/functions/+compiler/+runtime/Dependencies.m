%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef Dependencies
  properties (SetAccess = private)
    Required = []
    Optional = []
  end
  properties (Access = private)
    Snapshot = struct()
  end
  methods
    function obj = Dependencies(snapshot)
      narginchk(1, 1);
      if ~isstruct(snapshot) || ~isscalar(snapshot) ...
        || ~all(isfield(snapshot, {'files', 'moduleOrder', 'inputs', 'directories'})) ...
        || isempty(snapshot.files) || ~isstruct(snapshot.files) ...
        || ~all(isfield(snapshot.files, {'source', 'relative', 'sha256'}))
        error('Nelson:nelsonc:result:completeRuntimeFileSnapshotRequired', 'A complete runtime file snapshot is required.');
      end
      obj.Snapshot = snapshot;
      obj.Required = table({snapshot.files.source}', {snapshot.files.relative}', ...
        {snapshot.files.sha256}', 'VariableNames', {'Source', 'RelativePath', 'SHA256'});
      % The minimal closure has no additional optional runtime files.
      obj.Optional = obj.Required([], :);
    end
    function snapshot = __nelsonc_snapshot__(obj)
      snapshot = obj.Snapshot;
    end
  end
end
