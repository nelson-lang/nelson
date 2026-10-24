%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function plan = __nelsonc_link_plan__(entry, included, detectData, selection)
  selection = __nelsonc_option_value__('SupportPackages', selection);
  registry = nmm('list');
  catalog = __nelsonc_package_registry__(registry);
  names = {catalog.Name};
  excluded = false(size(names));
  if isequal(selection, {'none'})
    excluded(:) = true;
  elseif ~isequal(selection, {'autodetect'})
    missing = selection(~ismember(selection, names));
    if ~isempty(missing)
      error('Nelson:nelsonc:packages', 'Support package is not installed: %s.', strjoin(missing, ', '));
    end
    excluded = ~ismember(names, selection);
  end
  if ~isequal(selection, {'autodetect'})
    lookup = {};
    active = {};
    for k = 1:numel(catalog)
      if any(strcmp(active, catalog(k).Name)); continue; end
      active{end + 1} = catalog(k).Name;
      root = fullfile(catalog(k).Root, 'functions');
      if ~isfolder(root); root = catalog(k).Root; end
      lookup{end + 1} = root;
    end
    policy = struct('roots', {{catalog(excluded).Root}}, 'modules', {unique(names(excluded))}, 'lookup', {lookup});
    plan = jsondecode(__nelsonc_plan__('link', entry, included, detectData, jsonencode(policy)));
  else
    plan = jsondecode(__nelsonc_plan__('link', entry, included, detectData));
  end
  plan.supportPackages = selection;
  plan.includedSupportPackages = __nelsonc_included_packages__(plan, registry);
end
