/*
 * Requires uri parsing helpers from:
 * http://sourceforge.net/tracker/?func=detail&aid=3037660&group_id=50884&atid=461324
 */
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/http.h>
#include <event2/http_struct.h>

struct download_context
{
	struct evhttp_uri *uri;
	struct event_base *base;
	struct evhttp_connection *cn;
	struct evhttp_request *req;

	struct evbuffer *buffer;
	int ok;
};

static
void download_callback(struct evhttp_request *req, void *arg);

static
int download_renew_request(struct download_context *ctx);

static
void download_callback(struct evhttp_request *req, void *arg)
{
	struct download_context *ctx = (struct download_context *)arg;
	struct evhttp_uri *new_uri = NULL;
	const char *new_location = NULL;

	/* response is ready */

	switch(req->response_code)
	{
	case HTTP_OK:
		/* 
		 * Response is received. No futher handling is required.
		 * Finish
		 */
		event_base_loopexit(ctx->base, 0);
		break;

	case HTTP_MOVEPERM:
	case HTTP_MOVETEMP:
		new_location = evhttp_find_header(req->input_headers, "Location");
		if (!new_location)
			return;

		new_uri = evhttp_uri_parse(new_location);
		if (!new_uri)
			return;

		evhttp_uri_free(ctx->uri);
		ctx->uri = new_uri;

		download_renew_request(ctx);
		return;

	default:
		/* FAILURE */
		event_base_loopexit(ctx->base, 0);
		return;
	}

	evbuffer_add_buffer(ctx->buffer, req->input_buffer);

	/* SUCCESS */
	ctx->ok = 1;
}

struct download_context *context_new(const char *url)
{
	struct download_context *ctx = 0;
	ctx = calloc(1, sizeof(*ctx));
	if (!ctx)
		return 0;

	ctx->uri = evhttp_uri_parse(url);
	if (!ctx->uri)
		return 0;

	ctx->base = event_base_new();
	if (!ctx->base)
		return 0;

	ctx->buffer = evbuffer_new();

	download_renew_request(ctx);

	return ctx;
}

void context_free(struct download_context *ctx)
{
	evhttp_connection_free(ctx->cn);
	event_base_free(ctx->base);

	if (ctx->buffer)
		evbuffer_free(ctx->buffer);

	evhttp_uri_free(ctx->uri);
	free(ctx);
}

static
int download_renew_request(struct download_context *ctx)
{
	/* free connections & request */
	if (ctx->cn)
		evhttp_connection_free(ctx->cn);

	ctx->cn = evhttp_connection_base_new(
		ctx->base, NULL, 
		ctx->uri->host,
		ctx->uri->port ? ctx->uri->port : 80);

	ctx->req = evhttp_request_new(download_callback, ctx);

	evhttp_make_request(ctx->cn, ctx->req, EVHTTP_REQ_GET,
		ctx->uri->query ? ctx->uri->query : "/");

	evhttp_add_header(ctx->req->output_headers,
                            "Host", ctx->uri->host);
	return 0;
}

struct evbuffer *download_url(const char *url)
{
	/* setup request, connection etc */

	struct download_context *ctx = context_new(url);
	if (!ctx)
		return 0;

	/* do all of the job */
	event_base_dispatch(ctx->base);

	struct evbuffer *retval = 0;
	if (ctx->ok)
	{
		retval = ctx->buffer;
		ctx->buffer = 0;
	}

	context_free(ctx);

	return retval;
}

int main(int argc, char *argv[])
{
	if (argc < 2)
	{
		printf("usage: %s http://example.com/\n", argv[0]);
		return 1;
	}

	struct evbuffer *data = download_url(argv[1]);

	printf("got %d bytes\n", data ? evbuffer_get_length(data) : -1);

	if (data)
	{
		const char *joined = evbuffer_pullup(data, -1);
		printf("data itself:\n====================\n");
		write(1, joined, evbuffer_get_length(data));
		printf("\n====================\n");

		evbuffer_free(data);
	}

	return 0;
}
