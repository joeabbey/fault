import { useState, useEffect } from 'react';
import type { FC, ReactNode } from 'react';
import * as utils from './utils';
import defaultExport from 'some-module';
import 'side-effect-module';

export interface UserProps {
  name: string;
  age: number;
}

export type Status = 'active' | 'inactive';

export class UserService {
  private baseUrl: string;

  constructor(baseUrl: string) {
    this.baseUrl = baseUrl;
  }

  async getUser(id: number): Promise<UserProps> {
    const response = await fetch(`${this.baseUrl}/users/${id}`);
    return response.json();
  }
}

export function formatName(first: string, last: string): string {
  return `${first} ${last}`;
}

export async function fetchData(url: string): Promise<unknown> {
  const response = await fetch(url);
  return response.json();
}

export const MAX_RETRIES = 3;

export const processItems = (items: string[]) => {
  return items.map(item => item.trim());
};

const internalHelper = (x: number) => x * 2;

function privateFunc(): void {
  console.log('private');
}

export default class App {
  render() {
    return null;
  }
}
