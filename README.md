# waservix: WAservix WhatsApp API Gateway for Haskell

**WAservix WhatsApp API Gateway** - official Haskell client by Tekservix. Send WhatsApp texts, images, documents and locations from your own number.

**You need a Tekservix account with WAservix and a WhatsApp number connected in the dashboard.** Each number has its own API key (it starts with `wpk_`) under WAservix → Devices. [Create an account](https://tekservix.com/waservix) · [API docs](https://tekservix.com/waservix/docs/)

## Install

```
cabal install --lib waservix
```

or add `waservix` to `build-depends`.

## Use

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.WAservix

main :: IO ()
main = do
  c <- newClient "wpk_your_device_key"

  r <- sendText c "+250788123456" "Your order #1042 has shipped."
  _ <- sendImage c "+250788123456" "https://example.com/catalogue.jpg" (Just "New arrivals")
  _ <- sendDocument c "+250788123456" "https://example.com/inv-1042.pdf" (Just "Invoice-1042.pdf") (Just "Your invoice")
  _ <- sendLocation c "+250788123456" (-1.9441) 30.0619 (Just "Our shop")

  -- Groups: IDs end in @g.us
  groups <- listGroups c
  status <- getStatus c
  print (r, groups, status)
```

Results are aeson `Value`s (the API's `data` field). Errors are thrown as `WAservixError` with the HTTP status and message.

Webhooks: `verifyWebhook signatureHeader secret` checks `X-Webhook-Signature`.

## More WAservix integrations

[npm](https://www.npmjs.com/package/waservix) · [PyPI](https://pypi.org/project/waservix/) · [Java/Kotlin](https://central.sonatype.com/artifact/com.tekservix/waservix) · [All docs](https://tekservix.com/waservix/docs/)

support@tekservix.com · MIT licensed.
