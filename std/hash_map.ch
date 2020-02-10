let default_bucket_size: Int = 8.
let threshold: Float = 0.75.

object HashMap<_K, _V> {
  size: Int.

  buckets: [Bucket<_K, _V>].
  num_buckets: Int.
}

impl<_K, _V> for HashMap<_K, _V> {
  fn new() -> HashMap<_K, _V> {
    allocate HashMap {
      size: 0,

      buckets: allocate [Bucket; default_bucket_size],
      num_buckets: default_bucket_size
    }
  }

  fn bucket_from_hash(self, hash: Int) -> Bucket<_K, _V> {
    self:buckets[abs(hash % self:num_buckets)]
  }
}

impl<_K, _V> for HashMap<_K, _V> where _K: Equals<_K> + Hash {
  fn get(self, key: _K) -> Option<_V> {
    let hash = key:hash().

    for (maybe_hash, maybe_key, maybe_value) in self:bucket_from_hash(hash):entries {
      if hash == maybe_hash {
        if key == maybe_key {
          return Option!Some(maybe_value).
        }
      }
    }

    Option!None
  }

  fn remove(self, key: _K) -> Option<_V> {
    let hash = key:hash().
    let bucket = self:bucket_from_hash(hash).

    let next_link = bucket:entries:root.
    while true {
      match next_link {
        Option!Some(link) => {
          let (old_hash, old_key, old_value) = link:get().

          if hash == old_hash {
            if key == old_key {
              link:unlink().
              self:size = self:size - 1.
              return Option!Some(old_value).
            }
          }

          next_link = link:next.
        },
        Option!None => {
          break.
        },
      }
    }

    Option!None
  }

  fn put(self, key: _K, value: _V) -> Option<_V> {
    let hash = key:hash().
    let bucket = self:bucket_from_hash(hash).

    let next_link = bucket:entries:root.
    while true {
      match next_link {
        Option!Some(link) => {
          let (old_hash, old_key, old_value) = link:get().

          if hash == old_hash {
            if key == old_key {
              link:set((hash, key, value)).
              return Option!Some(old_value).
            }
          }

          next_link = link:next.
        },
        Option!None => {
          break.
        },
      }
    }

    self:size = self:size + 1.
    bucket:entries:push_back((hash, key, value)).

    self:try_grow().
    Option!None
  }

  fn try_grow(self) {
    // TODO: Let's integrate this into some type of ensure_size or resize or smth.

    if (self:size as Float) > (self:num_buckets as Float) * threshold {
      let old_buckets = self:buckets.
      self:num_buckets = self:num_buckets * 2.
      self:buckets = allocate [Bucket; self:num_buckets].

      for bucket in old_buckets {
        for (hash, key, value) in bucket:entries {
          self:bucket_from_hash(hash):entries:push_back((hash, key, value)).
        }
      }
    }
  }
}

impl<_K, _V> Len for HashMap<_K, _V> {
  fn len(self) -> Int = self:size.
}

impl<_K, _V> Deref for HashMap<_K, _V> where _K: Equals<_K> + Hash {
  type Idx = _K.
  type Result = _V.

  fn deref(self, idx: _K) -> _V {
    match self:get(idx) {
      Option!Some(v) => v,
      Option!None => panic("No such index"),
    }
  }
}

impl<_K, _V> DerefAssign for HashMap<_K, _V> where _K: Equals<_K> + Hash {
  type Idx = _K.
  type Value = _V.

  fn deref_assign(self, idx: _K, value: _V) -> _V {
    self:put(idx, value).
    value
  }
}

impl<_K, _V> Iterable for HashMap<_K, _V> {
  type Iterator = HashMapIterator<_K, _V>.
  type Item = (_K, _V).

  fn iterator(self) -> HashMapIterator<_K, _V> {
    let it = allocate HashMapIterator {
      size_hint: self:len(),
      next_bucket_idx: 0,
      buckets: self:buckets,
      next_link: Option!None
    }.

    it:bump().

    it
  }
}

object HashMapIterator<_K, _V> {
  size_hint: Int.
  buckets: [Bucket<_K, _V>].

  next_bucket_idx: Int.
  next_link: Option<Link<(Int, _K, _V)>>.
}

impl<_K, _V> for HashMapIterator<_K, _V> {
  fn bump(self) {
    match self:next_link {
      Option!Some(next_link) => {
        // Just bump up the linked list.
        if next_link:next:is_some() {
          self:next_link = next_link:next.
          return.
        }
      },
      Option!None => {}
    }

    while self:next_bucket_idx < self:buckets:len() {
      let bucket = self:buckets[self:next_bucket_idx].
      self:next_bucket_idx = self:next_bucket_idx + 1.

      if bucket:entries:len() > 0 {
        self:next_link = bucket:entries:root.
        return.
      }
    }

    self:next_link = Option!None.
  }
}

impl<_K, _V> Iterator for HashMapIterator<_K, _V> {
  type Item = (_K, _V).

  fn next(self) -> (_K, _V) {
    match self:next_link {
      Option!Some(next_link) => {
        let (_, k, v) = next_link:get().
        self:bump().
        self:size_hint = self:size_hint - 1.
        (k, v)
      },
      Option!None =>
        panic("Calling HashMapIterator:next() after it has been exhausted"),
    }
  }

  fn has_next(self) -> Bool = self:next_link:is_some().
  fn size_hint(self) -> Int = self:size_hint.
}

impl<_K, _V> Into<String> for HashMap<_K, _V> where _K: Into<String>, _V: Into<String> {
  fn into(self) -> String {
      let s = "HashMap{".
      let first = true.

      for (k, v) in self {
          if first {
              first = false.
          } else {
              s = s + ", ".
          }

          s = s + (k as String) + ": " + (v as String).
      }

      s + "}"
  }
}

object Bucket<_K, _V> {
  entries: List<(Int, _K, _V)>.
}

impl<_K, _V> Default for Bucket<_K, _V> {
  fn default() -> Bucket<_K, _V> =
    allocate Bucket { entries: List:new() }.
}
