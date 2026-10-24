//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
//=============================================================================
const roots = new WeakMap<Element, Root>();
export type SubsystemBreadcrumbItem = {
  name: string;
  levelsUp: number;
};
//=============================================================================
function SubsystemBreadcrumb({
  hidden,
  items,
  current,
  onNavigate,
}: {
  hidden: boolean;
  items: SubsystemBreadcrumbItem[];
  current: string;
  onNavigate: (levelsUp: number) => void;
}): JSX.Element {
  return (
    <nav
      id="subsystemBreadcrumb"
      className="subsystem-breadcrumb"
      aria-label="Subsystem path"
      hidden={hidden}
    >
      {!hidden
        ? items.map((item) => (
            <span className="breadcrumb-segment" key={`${item.name}:${item.levelsUp}`}>
              <button
                type="button"
                className="breadcrumb-item"
                title={item.name}
                onClick={() => onNavigate(item.levelsUp)}
              >
                {item.name}
              </button>
              <span className="breadcrumb-sep">{'\u203A'}</span>
            </span>
          ))
        : null}
      {!hidden ? <span className="breadcrumb-current">{current}</span> : null}
    </nav>
  );
}
//=============================================================================
export function renderSubsystemBreadcrumb(
  container: Element,
  hidden: boolean,
  items: SubsystemBreadcrumbItem[],
  current: string,
  onNavigate: (levelsUp: number) => void
): void {
  let root = roots.get(container);
  if (!root) {
    if (container.childNodes.length) container.replaceChildren();
    root = createRoot(container);
    roots.set(container, root);
  }
  flushSync(() => {
    root?.render(
      <SubsystemBreadcrumb
        hidden={hidden}
        items={items}
        current={current}
        onNavigate={onNavigate}
      />
    );
  });
}
//=============================================================================
export default renderSubsystemBreadcrumb;
//=============================================================================
