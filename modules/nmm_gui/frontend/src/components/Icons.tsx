//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Inline SVG icons of the package manager window (stroked with currentColor).
import type { CSSProperties, ReactNode } from 'react';
//=============================================================================
function Svg({ size, children, style, className }: { size: number; children: ReactNode; style?: CSSProperties; className?: string }) {
  return <svg className={className} width={size} height={size} viewBox="0 0 24 24" style={style} aria-hidden="true">{children}</svg>;
}
//=============================================================================
const box = (
  <>
    <path d="M12 3l8 4.5v9L12 21l-8-4.5v-9L12 3z" />
    <path d="M4 7.5l8 4.5 8-4.5" />
    <path d="M12 12v9" />
  </>
);
//=============================================================================
export const IconPackage = ({ size = 16, className, style }: { size?: number; className?: string; style?: CSSProperties }) => (
  <Svg size={size} className={className} style={style}>{box}</Svg>
);
//=============================================================================
export const IconCheck = () => <Svg size={12} style={{ strokeWidth: 2.2 }}><path d="M5 12l5 5L20 7" /></Svg>;
export const IconInstall = () => <Svg size={14}><path d="M12 4v11" /><path d="M7 10l5 5 5-5" /><path d="M4 19h16" /></Svg>;
export const IconUpdate = () => <Svg size={14}><path d="M12 20V9" /><path d="M7 14l5-5 5 5" /><path d="M4 5h16" /></Svg>;
export const IconRemove = () => (
  <Svg size={14}><path d="M4 7h16" /><path d="M10 11v6" /><path d="M14 11v6" /><path d="M6 7l1 13h10l1-13" /><path d="M9 7V4h6v3" /></Svg>
);
//=============================================================================
export const IconExternal = () => (
  <Svg size={12}><path d="M14 4h6v6" /><path d="M20 4l-9 9" /><path d="M19 13v6a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V6a1 1 0 0 1 1-1h6" /></Svg>
);
//=============================================================================
export const IconRefresh = () => <Svg size={14}><path d="M20 12a8 8 0 1 1-2.3-5.7" /><path d="M20 4v5h-5" /></Svg>;
export const IconCopy = () => <Svg size={14}><rect x="9" y="9" width="11" height="11" rx="2" /><path d="M5 15V5a2 2 0 0 1 2-2h10" /></Svg>;
export const IconSearch = () => <Svg size={14}><circle cx="11" cy="11" r="7" /><path d="M20 20l-3.5-3.5" /></Svg>;
export const IconPlus = () => <Svg size={14}><path d="M12 5v14" /><path d="M5 12h14" /></Svg>;
export const IconArchive = () => <Svg size={14}><path d="M21 8v13H3V8" /><path d="M1 3h22v5H1z" /><path d="M10 12h4" /></Svg>;
export const IconDatabase = () => (
  <Svg size={14}>
    <ellipse cx="12" cy="5" rx="8" ry="3" />
    <path d="M4 5v6c0 1.66 3.58 3 8 3s8-1.34 8-3V5" />
    <path d="M4 11v6c0 1.66 3.58 3 8 3s8-1.34 8-3v-6" />
  </Svg>
);
//=============================================================================
export const IconNoMatch = () => (
  <Svg size={28} style={{ strokeWidth: 1.5 }}><circle cx="11" cy="11" r="7" /><path d="M20 20l-3.5-3.5" /><path d="M8 11h6" /></Svg>
);
//=============================================================================
