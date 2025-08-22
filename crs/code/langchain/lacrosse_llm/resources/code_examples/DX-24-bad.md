``` C
void f(pthread_mutex_t *mutex) {
  pthread_mutex_lock(mutex);

  /* access shared resource */


  pthread_mutex_unlock(mutex);
}
```
