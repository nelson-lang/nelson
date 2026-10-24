//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
type Seq = { __seq?: number };
export class MinHeap<T extends { f: number } = { idx: number; f: number }> {
  items: (T & Seq)[];
  private counter: number;
  constructor() {
    this.items = [];
    this.counter = 0;
  }
  //=============================================================================
  size(): number {
    return this.items.length;
  }
  //=============================================================================
  push(item: T): void {
    // Insertion-order sequence number breaks ties on equal `f` so pop order
    // is fully deterministic (and identical to the native router's heap).
    (item as T & Seq).__seq = this.counter++;
    this.items.push(item as T & Seq);
    this.bubbleUp(this.items.length - 1);
  }
  //=============================================================================
  pop(): T {
    if (this.items.length === 0) throw new Error('pop from empty heap');
    if (this.items.length === 1) return this.items.pop() as T;
    const top = this.items[0];
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    this.items[0] = this.items.pop()!;
    this.bubbleDown(0);
    return top;
  }
  //=============================================================================
  /** Strict ordering: primary `f`, ties broken by insertion sequence. */
  private less(a: T & Seq, b: T & Seq): boolean {
    if (a.f !== b.f) return a.f < b.f;
    return (a.__seq ?? 0) < (b.__seq ?? 0);
  }
  //=============================================================================
  bubbleUp(index: number): void {
    let idx = index;
    while (idx > 0) {
      const parent = Math.floor((idx - 1) / 2);
      if (!this.less(this.items[idx], this.items[parent])) break;
      [this.items[parent], this.items[idx]] = [this.items[idx], this.items[parent]];
      idx = parent;
    }
  }
  //=============================================================================
  bubbleDown(index: number): void {
    let idx = index;
    const length = this.items.length;
    while (true) {
      const left = idx * 2 + 1;
      const right = idx * 2 + 2;
      let smallest = idx;
      if (left < length && this.less(this.items[left], this.items[smallest])) smallest = left;
      if (right < length && this.less(this.items[right], this.items[smallest])) smallest = right;
      if (smallest === idx) break;
      [this.items[smallest], this.items[idx]] = [this.items[idx], this.items[smallest]];
      idx = smallest;
    }
  }
}
//=============================================================================
