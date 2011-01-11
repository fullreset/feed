#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include <curl/curl.h>

#include <feed.h>

#define BUF_INIT 1024
#define BUF_MAX 1073741824

uw_unit uw_FeedFfi_init(uw_context ctx) {
  curl_global_init(CURL_GLOBAL_ALL);

  return uw_unit_v;
}

static CURL *curl(uw_context ctx) {
  CURL *r;

  if (!(r = uw_get_global(ctx, "curl"))) {
    r = curl_easy_init();
    if (r)
      uw_set_global(ctx, "curl", r, curl_easy_cleanup);
  }

  return r;
}

static void free_buffer(void *data) {
  uw_buffer *buf = data;

  uw_buffer_free(buf);
  free(buf);
}

static uw_buffer *curl_buffer(uw_context ctx) {
  uw_buffer *r;

  if (!(r = uw_get_global(ctx, "curl_buffer"))) {
    r = malloc(sizeof(uw_buffer));
    if (r) {
      uw_buffer_init(BUF_MAX, r, BUF_INIT);
      uw_set_global(ctx, "curl_buffer", r, free_buffer);
    }
  }

  return r;
}

static size_t write_data(void *buffer, size_t size, size_t nmemb, void *userp) {
  uw_buffer *out = userp;

  uw_buffer_append(out, buffer, size * nmemb);

  return size * nmemb;
}

uw_Basis_string uw_FeedFfi_fetch(uw_context ctx, uw_Basis_string url) {
  CURL *c = curl(ctx);
  uw_buffer *b = curl_buffer(ctx);
  CURLcode code;

  uw_buffer_reset(b);

  curl_easy_setopt(c, CURLOPT_URL, url);
  curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, write_data);
  curl_easy_setopt(c, CURLOPT_WRITEDATA, b);

  code = curl_easy_perform(c);

  if (code)
    uw_error(ctx, FATAL, "Error fetching URL %s", url);
  else {
    uw_buffer_append(b, "", 1);
    return uw_strdup(ctx, b->start);
  }
}
