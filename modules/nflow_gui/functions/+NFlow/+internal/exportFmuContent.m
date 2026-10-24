%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fmuPath = exportFmuContent(nflowContent, fmuFile)
%
% Export a diagram (given as its .nflow JSON content) to an FMI 3.0 Co-Simulation
% FMU at the target path and return it. Internal RPC helper around the public
% NFlow.exportfmu, invoked by the editor's Export-as-FMU action from the
% interpreter thread: it stages the content to a temporary .nflow, exports it,
% and moves the produced archive to the chosen path. External ports
% (labelSource/labelSink) become the FMU's Float64 inputs/outputs. The FMU
% carries a compiled binary when a C toolchain is present, else source-only.
%=============================================================================
function fmuPath = exportFmuContent(nflowContent, fmuFile)
  narginchk(2, 2);
  tmp = [tempname(), '.nflow'];
  fid = fopen(tmp, 'wt');
  if fid == -1
    error('Nelson:nflow_gui:exportFmu', 'Could not stage a temporary model for FMU export.');
  end
  fwrite(fid, nflowContent);
  fclose(fid);
  cleanup = onCleanup(@() export_fmu_delete(tmp)); %#ok<NASGU>
  destDir = fileparts(fmuFile);
  if isempty(destDir)
    destDir = pwd();
  end
  produced = NFlow.exportfmu(tmp, destDir);
  % NFlow.exportfmu names the archive after the (temporary) model; move it to the
  % path the user chose.
  produced = char(produced);
  fmuFile = char(fmuFile);
  if ~strcmp(produced, fmuFile)
    if isfile(fmuFile)
      delete(fmuFile);
    end
    copyfile(produced, fmuFile);
    if isfile(produced)
      delete(produced);
    end
  end
  fmuPath = fmuFile;
end
%=============================================================================
function export_fmu_delete(p)
  if isfile(p)
    delete(p);
  end
end
%=============================================================================
