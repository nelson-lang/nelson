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
figure();
p = patch();
names = properties(p);
REF = {'AlignVertexCenters';
  'AlphaDataMapping';
  'AmbientStrength';
  'Annotation';
  'BackFaceLighting';
  'BeingDeleted';
  'BusyAction';
  'ButtonDownFcn';
  'CData';
  'CDataMapping';
  'CDataMode';
  'Children';
  'Clipping';
  'ContextMenu';
  'CreateFcn';
  'DeleteFcn';
  'DiffuseStrength';
  'DisplayName';
  'EdgeAlpha';
  'EdgeColor';
  'EdgeLighting';
  'FaceAlpha';
  'FaceColor';
  'FaceLighting';
  'FaceNormals';
  'FaceNormalsMode';
  'FaceVertexAlphaData';
  'FaceVertexCData';
  'FaceVertexCDataMode';
  'Faces';
  'HandleVisibility';
  'HitTest';
  'Interruptible';
  'LineJoin';
  'LineStyle';
  'LineWidth';
  'Marker';
  'MarkerEdgeColor';
  'MarkerFaceColor';
  'MarkerSize';
  'Parent';
  'PickableParts';
  'Selected';
  'SelectionHighlight';
  'SeriesIndex';
  'SpecularColorReflectance';
  'SpecularExponent';
  'SpecularStrength';
  'Tag';
  'Type';
  'UserData';
  'VertexNormals';
  'VertexNormalsMode';
  'Vertices';
  'Visible';
  'XData';
  'YData';
  'ZData'};
asserts.istrue(all(ismember(REF, names)));
asserts.istrue(p.Visible == 'on');
asserts.isequal(p.XData, [0; 0; 1; 0]);
asserts.isequal(p.YData, [1; 1; 1; 0]);
asserts.isequal(p.ZData, [1; 1; 1; 1]);
asserts.isequal(p.LineStyle, '-');
p.FaceColor = 'none';
asserts.isequal(p.FaceColor, 'none');
p.EdgeAlpha = 0.4;
asserts.isapprox(p.EdgeAlpha, 0.4, 1e-12);
p.LineJoin = 'miter';
asserts.isequal(p.LineJoin, 'miter');
p.Marker = 'o';
p.MarkerEdgeColor = [1 0 0];
p.MarkerFaceColor = 'none';
p.MarkerSize = 8;
asserts.isequal(p.Marker, 'o');
asserts.isequal(p.MarkerEdgeColor, [1 0 0]);
asserts.isequal(p.MarkerFaceColor, 'none');
asserts.isequal(p.MarkerSize, 8);
%=============================================================================
