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
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
s = surf(ax, peaks(5));
asserts.isapprox(s.AmbientStrength, 0.3, 1e-12);
asserts.isapprox(s.DiffuseStrength, 0.6, 1e-12);
asserts.isapprox(s.SpecularStrength, 0.9, 1e-12);
asserts.isapprox(s.SpecularExponent, 10, 1e-12);
asserts.isapprox(s.SpecularColorReflectance, 1, 1e-12);
shading(ax, 'flat');
asserts.isequal(s.FaceColor, 'flat');
asserts.isequal(s.EdgeColor, 'none');
shading(ax, 'interp');
asserts.isequal(s.FaceColor, 'interp');
asserts.isequal(s.EdgeColor, 'none');
shading(ax, 'faceted');
asserts.isequal(s.FaceColor, 'flat');
asserts.isequal(s.EdgeColor, [0 0 0]);
%=============================================================================
lighting(ax, 'gouraud');
asserts.isequal(s.FaceLighting, 'gouraud');
asserts.isequal(s.EdgeLighting, 'gouraud');
lighting(ax, 'none');
asserts.isequal(s.FaceLighting, 'none');
asserts.isequal(s.EdgeLighting, 'none');
lighting(ax, 'phong');
asserts.isequal(s.FaceLighting, 'phong');
asserts.isequal(s.EdgeLighting, 'phong');
asserts.checkerror('lighting(ax, ''bogus'')', getString(message('Nelson:lighting:invalidPropertyValue')));
%=============================================================================
material(ax, 'shiny');
asserts.isapprox(s.AmbientStrength, 0.3, 1e-12);
asserts.isapprox(s.DiffuseStrength, 0.6, 1e-12);
asserts.isapprox(s.SpecularStrength, 0.9, 1e-12);
asserts.isapprox(s.SpecularExponent, 20, 1e-12);
asserts.isapprox(s.SpecularColorReflectance, 1, 1e-12);
material(ax, [0.2 0.7 0.1 8 0.5]);
asserts.isapprox(s.AmbientStrength, 0.2, 1e-12);
asserts.isapprox(s.DiffuseStrength, 0.7, 1e-12);
asserts.isapprox(s.SpecularStrength, 0.1, 1e-12);
asserts.isapprox(s.SpecularExponent, 8, 1e-12);
asserts.isapprox(s.SpecularColorReflectance, 0.5, 1e-12);
%=============================================================================
% a three-element vector [ka kd ks] sets the three strengths and leaves the
% specular exponent and color reflectance untouched.
material(ax, [0.4 0.5 0.6]);
asserts.isapprox(s.AmbientStrength, 0.4, 1e-12);
asserts.isapprox(s.DiffuseStrength, 0.5, 1e-12);
asserts.isapprox(s.SpecularStrength, 0.6, 1e-12);
asserts.isapprox(s.SpecularExponent, 8, 1e-12);
asserts.isapprox(s.SpecularColorReflectance, 0.5, 1e-12);
%=============================================================================
vn = zeros(5, 5, 3);
vn(:, :, 3) = 1;
s.VertexNormals = vn;
s.VertexNormalsMode = 'manual';
asserts.isequal(s.VertexNormalsMode, 'manual');
asserts.isequal(size(s.VertexNormals), [5 5 3]);
%=============================================================================
p = patch(ax, [0 1 0], [0 0 1], [0 0 0], 'r');
p.FaceNormals = [0 0 1];
p.FaceNormalsMode = 'manual';
p.VertexNormals = [0 0 1; 0 0 1; 0 0 1];
p.VertexNormalsMode = 'manual';
asserts.isequal(p.FaceNormalsMode, 'manual');
asserts.isequal(p.VertexNormalsMode, 'manual');
close all
%=============================================================================
% material accepts an explicit object (or object array) target, not only an axes
f2 = figure('Visible', 'off');
s1 = surf(peaks(8));
s2 = surf(peaks(8));
material(s1, [0.1 0.2 0.3 4 0.5]);
asserts.isapprox(s1.AmbientStrength, 0.1, 1e-12);
asserts.isapprox(s1.SpecularExponent, 4, 1e-12);
material([s1; s2], [0.6 0.6 1 1 1]);
asserts.isapprox(s1.DiffuseStrength, 0.6, 1e-12);
asserts.isapprox(s2.SpecularStrength, 1, 1e-12);
material(s2, 'metal');
asserts.isapprox(s2.SpecularColorReflectance, 0.5, 1e-12);
close(f2);
%=============================================================================
