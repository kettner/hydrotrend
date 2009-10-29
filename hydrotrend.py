#! /usr/bin/env python

import hydrotrend_mod

class HydrotrendError (Exception):
  pass

class NotInitializedError (Exception):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return repr (self.value)

class Hydrotrend (object):
  def __init__ (self, in_dir=".", data_prefix='HYDRO', out_dir="."):
    self._in_dir = in_dir
    self._out_dir = out_dir
    self._data_prefix = data_prefix

    self._state = hydrotrend_mod.ht_initialize (in_dir, data_prefix, out_dir)
    
  def init (self):
    if self._state is None:
      self._state = hydrotrend_mod.ht_initialize (in_dir, data_prefix, out_dir)

  def run (self, time):
    if self._state is not None:
      hydrotrend_mod.ht_run_until (self._state, time)
    else:
      raise NotInitializedError ('Trying to run uninitialized model')
    
  def finalize (self):
    if self._state is not None:
      self._state = hydrotrend_mod.ht_finalize (self._state)
    else:
      raise NotInitializedError ('Trying to finalize uninitialized model')

