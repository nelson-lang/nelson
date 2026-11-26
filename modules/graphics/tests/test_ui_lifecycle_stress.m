%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Creating, introspecting and deleting graphics and UI objects in a loop must not
% crash: a WebView stop request once raced the host thread freeing it.
%=============================================================================
ctors = { ...
  'h = uicontrol(f)', 'h = uipanel(f)', 'h = uibuttongroup(f)', 'h = uitable(f)', ...
  'h = uimenu(f)', 'h = uicontextmenu(f)', 'h = uitabgroup(f)', ...
  'tg = uitabgroup(f); h = uitab(tg)', 'h = uf', 'h = uiaxes(uf)', 'h = uibutton(uf)', ...
  'h = uicheckbox(uf)', 'h = uidropdown(uf)', 'h = uilistbox(uf)', 'h = uitree(uf)', ...
  't = uitree(uf); h = uitreenode(t)', 'h = uigridlayout(uf)', 'h = uitable(uf)', ...
  'h = uimenu(uf)', 'h = uicontextmenu(uf)', 'delete(ax); figure(f); h = heatmap(magic(3))', ...
  'h = histogram(ax, randn(100,1))', 'h = binscatter(ax, randn(100,1), randn(100,1))'};
for r = 1:4
  for k = 1:numel(ctors)
    f = figure('Visible', 'off');
    ax = axes(f);
    uf = [];
    if contains(ctors{k}, 'uf')
      uf = uifigure('Visible', 'off');
    end
    eval([ctors{k} ';']);
    assert_istrue(isstruct(get(h(1))));
    assert_istrue(isstruct(set(h(1))));
    if ~isempty(uf)
      delete(uf);
    end
    delete(f);
    assert_isfalse(isvalid(f));
  end
end
%=============================================================================
