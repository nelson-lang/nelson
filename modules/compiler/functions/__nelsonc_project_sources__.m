%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = __nelsonc_project_sources__(project, plan)
  status = struct('State', 'unbuilt', 'Message', 'No saved build.');
  if nargin == 0 || isempty(project.LastBuild.ReportFile); return; end
  if isempty(project.LastBuild.SourcesSHA256)
    status.State = 'unknown';
    status.Message = 'Saved build; source fingerprints unavailable.';
    return;
  end
  if ~strcmp(project.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', project))
    status.State = 'changed';
    status.Message = 'Build settings changed; rebuild before creating an installer.';
    return;
  end
  try
    options = __nelsonc_project__('options', project);
    if nargin < 2
      plan = __nelsonc_link_plan__(options.AppFile, options.AdditionalFiles, options.AutoDetectDataFiles, options.SupportPackages);
      plan = __nelsonc_application_native__(plan);
    end
    if ~plan.complete
      error('Nelson:compiler:project:unresolvedDependencies', 'Unresolved dependencies: %s', jsonencode(plan.diagnostics));
    end
    helpText = '';
    if ~isempty(options.CustomHelpTextFile); helpText = __nelsonc_help_text__(options.CustomHelpTextFile); end
    iconDigest = '';
    if ~isempty(options.ExecutableIcon); iconDigest = sha256(options.ExecutableIcon, '-file'); end
    current = __nelsonc_source_fingerprint__(plan, helpText, iconDigest);
    if strcmp(current, project.LastBuild.SourcesSHA256)
      status.State = 'current';
      status.Message = 'Build sources unchanged.';
    else
      status.State = 'changed';
      status.Message = 'Build sources changed; rebuild before creating an installer.';
    end
  catch ex
    status.State = 'unavailable';
    status.Message = ['Saved build; source verification unavailable: ', ex.message];
  end
end
